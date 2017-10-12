{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | Exports a function to make client-side AJAX requests to the server that
-- provides the client.
module Servant.Client.Ghcjs.Extended
  ( callServant
  ) where

import qualified "ghcjs-base"            Data.JSString as JSS
import           "ghcjs-base"            JavaScript.Web.Location
import           "servant-client-ghcjs"  Servant.Client.Ghcjs

-- | Performs blocking AJAX request on the location of the browser window
callServant
    :: String
       -- ^ Path prefixed to HTTP requests.
    -> ClientM a
    -> IO (Either ServantError a)
callServant path m = do
    curLoc <- getWindowLocation

    jsStr_protocol <- getProtocol curLoc
    jsStr_port     <- getPort     curLoc
    jsStr_hostname <- getHostname curLoc

    let protocol
          | jsStr_protocol == "https:" = Https
          | otherwise                  = Http

        portStr :: String
        portStr = JSS.unpack jsStr_port

        port :: Int
        port | null portStr = case protocol of
                 Http  ->  80
                 Https -> 443
             | otherwise = read portStr

        hostname :: String
        hostname = JSS.unpack jsStr_hostname

    runClientM m (ClientEnv (BaseUrl protocol hostname port path))
