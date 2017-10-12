{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}

module Nixtodo.Api where

import           "servant"             Servant.API
import           "lens"                Control.Lens (makeLenses)
import qualified "text"                Data.Text as T
import           "base"                Data.List (stripPrefix)
import           "base"                Data.Char (isUpper, toLower)
import           "aeson"               Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON)
import           "aeson"               Data.Aeson.Types (Options, defaultOptions, fieldLabelModifier)
import           "base"                GHC.Generics (Generic)
import           "product-profunctors" Data.Profunctor.Product.TH ( makeAdaptorAndInstance )

--------------------------------------------------------------------------------
-- Endpoints
--------------------------------------------------------------------------------

type NixtodoApi =
       "entries" :> NixtodoRestApi
  :<|> "websocket" :> Raw
  :<|> FrontendApi

type NixtodoRestApi =
       CreateEntry
  :<|> ReadEntries
  :<|> UpdateEntry
  :<|> DeleteEntry

type CreateEntry =
     ReqBody '[JSON] EntryInfo
  :> Post '[JSON] Entry

type ReadEntries =
     Get '[JSON] [Entry]

type UpdateEntry =
     CaptureEntryId
  :> ReqBody '[JSON] EntryInfo
  :> Put '[JSON] NoContent

type DeleteEntry =
     CaptureEntryId
  :> Delete '[JSON] NoContent

type CaptureEntryId = Capture "eid" EntryId

type FrontendApi =
       GetStatic
  :<|> GetHashed
  :<|> GetIndex

type GetStatic = "static" :> Raw
type GetHashed = "hashed" :> Raw
type GetIndex  = Raw


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data EntryEvent =
      UpsertEntryEvent Entry
    | DeleteEntryEvent EntryId
      deriving (Show, Generic, Eq)

type EntryId = Int

type Entry = Entry' EntryId EntryInfo

data Entry' id entry =
     Entry
     { _entryId    :: !id
     , _entryEntry :: !entry
     } deriving (Show, Generic, Eq)

type EntryInfo = EntryInfo' T.Text Bool

data EntryInfo' description completed =
     EntryInfo
     { _entryInfDescription :: !description
     , _entryInfCompleted   :: !completed
     } deriving (Show, Generic, Eq)

instance ToJSON EntryEvent

instance FromJSON EntryEvent

instance ToJSON Entry where
    toJSON = genericToJSON $ optionsDelPrefix "_entry"

instance FromJSON Entry where
    parseJSON = genericParseJSON $ optionsDelPrefix "_entry"

instance ToJSON EntryInfo where
    toJSON = genericToJSON $ optionsDelPrefix "_entryInf"

instance FromJSON EntryInfo where
    parseJSON = genericParseJSON $ optionsDelPrefix "_entryInf"


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

optionsDelPrefix :: String -> Options
optionsDelPrefix prefix =
    defaultOptions{fieldLabelModifier = delPrefix prefix}

delPrefix :: String -> (String -> String)
delPrefix "" = id
delPrefix prefix = \fieldName ->
    case stripPrefix prefix fieldName of
      Just (c:cs)
          | isUpper c -> toLower c : cs
          | otherwise -> error $ "The field name after the prefix "
                              ++ "must be written in CamelCase"
      Just "" -> error $ "The field name after the prefix may not be empty"
      Nothing -> error $  "The field name " ++ quotes fieldName
                      ++ " does not begin with the required prefix "
                      ++ quotes prefix

quotes :: String -> String
quotes s = "\"" ++ s ++ "\""

makeLenses ''Entry'
makeLenses ''EntryInfo'

makeAdaptorAndInstance "pEntry"     ''Entry'
makeAdaptorAndInstance "pEntryInfo" ''EntryInfo'
