{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PackageImports      #-}

module Nixtodo.Api.Client where

import "base"                Data.Proxy
import "nixtodo-api"         Nixtodo.Api
import "servant-client-core" Servant.Client.Core
import "servant"             Servant.API

data NixtodoApiClient m
   = NixtodoApiClient
     { createEntry :: !(Client m CreateEntry)
     , readEntries :: !(Client m ReadEntries)
     , updateEntry :: !(Client m UpdateEntry)
     , deleteEntry :: !(Client m DeleteEntry)
     }

nixtodoApiClient :: forall m . HasClient m NixtodoApi => NixtodoApiClient m
nixtodoApiClient = NixtodoApiClient{..}
  where
    (      createEntry
      :<|> readEntries
      :<|> updateEntry
      :<|> deleteEntry
     ):<|> _websocketApi
      :<|> _frontendApi = Proxy @NixtodoApi `clientIn` Proxy @m
