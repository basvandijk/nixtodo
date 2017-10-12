{-# language PackageImports #-}
{-# language OverloadedStrings #-}

module Nixtodo.Backend.IndexTemplater
  ( Config(..)
  , parseConfig

  , Handle
  , getConfig
  , with

  , getHashed
  , getClient
  ) where

import qualified "SHA"               Data.Digest.Pure.SHA as SHA
import           "base"              Control.Concurrent.MVar (MVar, newMVar, withMVar)
import           "base"              Control.Monad (when)
import           "base"              Control.Monad.IO.Class (liftIO)
import           "base"              Data.Functor (void)
import           "base"              Data.Monoid ((<>))
import           "base16-bytestring" Data.ByteString.Base16.Lazy as Base16L
import qualified "bytestring"        Data.ByteString.Lazy as BL
import qualified "bytestring"        Data.ByteString.Lazy.Char8 as BLC8
import qualified "configurator"      Data.Configurator as C
import qualified "configurator"      Data.Configurator.Types as C
import qualified "directory"         System.Directory as Dir
import           "filepath"          System.FilePath ( (<.>), (</>) )
import qualified "filepath"          System.FilePath as Fp
import qualified "fsnotify"          System.FSNotify as FSNotify
import qualified "hastache"          Text.Hastache as H
import qualified "hastache"          Text.Hastache.Context as H
import qualified "http-types"        Network.HTTP.Types.Status as Http
import           "managed"           Control.Monad.Managed.Safe ( Managed, managed )
import qualified "servant-server"    Servant
import           "tagged"            Data.Tagged (Tagged(..))
import qualified "text"              Data.Text         as T
import qualified "text"              Data.Text.Lazy.IO as TL
import qualified "unix"              System.Posix.Files as Posix
import qualified "wai"               Network.Wai as Wai
import           "wai-app-static"    Network.Wai.Application.Static ( StaticSettings, defaultFileServerSettings, staticApp, ssMaxAge )
import           "wai-app-static"    WaiAppStatic.Types ( MaxAge(MaxAgeForever) )
import qualified "zlib"              Codec.Compression.GZip as Gz

data Config
   = Config
     { cfgIndexTemplatePath :: !FilePath
     , cfgSrcDir            :: !FilePath
     , cfgDstDir            :: !FilePath
     , cfgUrlPrefix         :: !FilePath
     , cfgCompressLevel     :: !Int
     }

parseConfig :: C.Config -> IO Config
parseConfig cfg = Config <$> C.require cfg "indexTemplatePath"
                         <*> C.require cfg "srcDir"
                         <*> C.require cfg "dstDir"
                         <*> C.require cfg "urlPrefix"
                         <*> C.require cfg "compressLevel"

data Handle
   = Handle
     { hndlConfig :: !Config
     , hndlLock   :: !(MVar ())
     }

getConfig :: Handle -> Config
getConfig = hndlConfig

with :: Config -> Managed Handle
with cfg = do
    watchManager <- managed FSNotify.withManager

    hndl <- liftIO initHndl

    liftIO $ do
      update hndl

      void $ FSNotify.watchTree
               watchManager
               (cfgSrcDir cfg)
               shouldHandleFsEvent
               (\_ -> update hndl)

    pure hndl
  where
    initHndl :: IO Handle
    initHndl = do
      lock <- newMVar ()
      pure Handle
           { hndlConfig = cfg
           , hndlLock   = lock
           }

    shouldHandleFsEvent :: FSNotify.Event -> Bool
    shouldHandleFsEvent FSNotify.Added{}    = False
    shouldHandleFsEvent FSNotify.Modified{} = True
    shouldHandleFsEvent FSNotify.Removed{}  = False

update :: Handle -> IO ()
update hndl = withMVar lock $ \_ -> do
    currentExists <- Dir.doesDirectoryExist currentLinkFp

    newRelative <- if currentExists
      then otherThen <$> Posix.readSymbolicLink currentLinkFp
      else pure "a"

    let new = dstDir </> newRelative

    newExists <- Dir.doesDirectoryExist new
    when newExists $ Dir.removeDirectoryRecursive new
    Dir.createDirectoryIfMissing True new

    let muContext :: H.MuContext IO
        muContext = H.mkStrContextM $ \fp -> do
          let fullFp = cfgSrcDir cfg </> fp

          bytes <- BL.readFile fullFp

          let compressed = Gz.compressWith compressParams bytes
              hash       = sha256sum compressed

              hashFpExt   = hash <> "-" <> Fp.takeFileName fp
              srcFp       = new </> hashFpExt
              srcFpGz     = srcFp <.> "gz"

          fullAbsFp <- Dir.makeAbsolute fullFp
          Posix.createSymbolicLink fullAbsFp srcFp
          BL.writeFile srcFpGz compressed

          pure $ H.MuVariable $ cfgUrlPrefix cfg </> hashFpExt

    indexTxt <- H.hastacheFile H.defaultConfig (cfgIndexTemplatePath cfg) muContext

    TL.writeFile (new </> indexFp) indexTxt

    newCurrentLinkExists <- Dir.doesDirectoryExist newCurrentLinkFp
    when newCurrentLinkExists $ Posix.removeLink newCurrentLinkFp
    Posix.createSymbolicLink newRelative newCurrentLinkFp

    Dir.renameFile newCurrentLinkFp currentLinkFp
  where
    currentLinkFp, newCurrentLinkFp :: FilePath
    currentLinkFp = dstDir </> currentFp
    newCurrentLinkFp = currentLinkFp <.> "new"

    otherThen :: FilePath -> FilePath
    otherThen "a" = "b"
    otherThen "b" = "a"
    otherThen _   = error "Invalid current link!"

    dstDir = cfgDstDir cfg

    compressParams :: Gz.CompressParams
    compressParams = Gz.defaultCompressParams
      { Gz.compressLevel       = Gz.compressionLevel $ cfgCompressLevel cfg
      , Gz.compressMemoryLevel = Gz.maxMemoryLevel
      }

    cfg    = hndlConfig hndl
    lock   = hndlLock   hndl

sha256sum :: BL.ByteString -> String
sha256sum = BLC8.unpack
          . BL.take 10
          . Base16L.encode
          . SHA.bytestringDigest
          . SHA.sha256

getHashed :: Handle -> Servant.Server Servant.Raw
getHashed hndl = Servant.Tagged $ staticApp staticSettings
  where
    staticSettings :: StaticSettings
    staticSettings = (defaultFileServerSettings
                       (Fp.addTrailingPathSeparator
                         (cfgDstDir cfg </> currentFp)))
        { ssMaxAge = MaxAgeForever }

    cfg = hndlConfig hndl

getClient :: Handle -> Servant.Server Servant.Raw
getClient hndl = Servant.Tagged $ requireEmptyPath $
    \_req respond -> do
      respond $ Wai.responseFile
        Http.ok200
        [ ("Cache-Control", "no-cache, no-store, must-revalidate")
        , ("Expires", "0")
        ]
        (cfgDstDir (hndlConfig hndl) </> currentFp </> indexFp)
        Nothing

currentFp :: FilePath
currentFp = "current"

indexFp :: FilePath
indexFp = "index.html"

requireEmptyPath :: Wai.Middleware
requireEmptyPath application =
    \req respond ->
      case Wai.pathInfo req of
        [] -> application req respond
        _  -> respond $ Wai.responseLBS Http.notFound404 [] "not found"
