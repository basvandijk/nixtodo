{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language PackageImports  #-}

module Nixtodo.Backend.Db.Types where

import           "opaleye"             Opaleye
import           "product-profunctors" Data.Profunctor.Product.TH ( makeAdaptorAndInstance )
import           "lens"                Control.Lens ( makeLenses )
import qualified "text"                Data.Text as T
import           "nixtodo-api"         Nixtodo.Api

type DBEntryInfo = EntryInfo'
                     (Column PGText)
                     (Column PGBool)

entriesTable :: Table (Entry' (Maybe (Column PGInt4)) DBEntryInfo)
                      (Entry' (Column PGInt4) DBEntryInfo)
entriesTable =
    Table "entries" $
      pEntry Entry
        { _entryId    = optional "id"
        , _entryEntry =
            pEntryInfo EntryInfo
              { _entryInfDescription = required "description"
              , _entryInfCompleted   = required "completed"
              }
        }
