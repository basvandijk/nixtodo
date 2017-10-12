{-# language OverloadedStrings #-}
{-# language PackageImports #-}

module Main (main) where

import           "base"                 Control.Applicative (many)
import           "base"                 Control.Monad.IO.Class (liftIO)
import           "base"                 Data.Monoid ((<>))
import qualified "configurator"         Data.Configurator as C
import qualified "configurator"         Data.Configurator.Types as C
import           "managed"              Control.Monad.Managed.Safe (runManaged, managed_)
import qualified "optparse-applicative" Options.Applicative as O
import qualified "text"                 Data.Text as T
import qualified "this"                 Nixtodo.Backend.Db as Db
import qualified "this"                 Nixtodo.Backend.IndexTemplater as IndexTemplater
import qualified "this"                 Nixtodo.Backend.WebServer as WebServer

main :: IO ()
main = runManaged $ do
    cfg <- liftIO $ do
      configFiles <- O.execParser opts
      C.load $ map C.Required configFiles

    let subCfg :: T.Text -> C.Config
        subCfg sectionName = C.subconfig sectionName cfg

    db <- do
      c <- liftIO $ Db.parseConfig (subCfg "db")
      Db.with c

    frontendIndexTemplater <- do
      c <- liftIO $ IndexTemplater.parseConfig $ subCfg "frontendIndexTemplater"
      IndexTemplater.with c

    liftIO $ do
      c <- WebServer.parseConfig (subCfg "web-server")
      WebServer.serve c db frontendIndexTemplater
  where
    opts :: O.ParserInfo [FilePath]
    opts = O.info (O.helper <*> options)
      (  O.fullDesc
      <> O.progDesc "The TODO-list backend server"
      )

    options :: O.Parser [FilePath]
    options = many (O.strOption (  O.long "config"
                                <> O.short 'c'
                                <> O.help "Configuration files to load"
                                )
                   )
