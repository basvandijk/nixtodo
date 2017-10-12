{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE NumDecimals                #-}
{-# LANGUAGE PackageImports             #-}

module Main where

import           "aeson"                Data.Aeson hiding (Object, (.=))
import           "base"                 Control.Concurrent (threadDelay)
import           "base"                 Control.Monad
import           "base"                 Data.Bool
import qualified "base"                 Data.Foldable as F (for_)
import           "base"                 Data.Function (on)
import           "base"                 Data.List
import           "base"                 Data.Maybe (listToMaybe)
import           "base"                 Data.Monoid
import           "base"                 GHC.Generics (Generic)
import qualified "containers"           Data.Map as M
import qualified "ghcjs-base"           Data.JSString as JSS
import           "ghcjs-base"           Data.JSString.Text (textFromJSString)
import qualified "ghcjs-base"           JavaScript.Web.Location
import           "ghcjs-dom-jsffi"      GHCJS.DOM (currentWindowUnchecked)
import           "ghcjs-dom-jsffi"      GHCJS.DOM.Location (getProtocol, getHostname, getPort)
import           "ghcjs-dom-jsffi"      GHCJS.DOM.Window (getLocation)
import qualified "lens"                 Control.Lens as L
import           "lens"                 Control.Lens hiding (view)
import           "miso"                 Miso hiding (on)
import           "miso"                 Miso.String (MisoString, toMisoString)
import qualified "miso"                 Miso.String as S
import           "nixtodo-api"          Nixtodo.Api
import           "nixtodo-api-client"   Nixtodo.Api.Client
import           "servant-client-ghcjs" Servant.Client.Ghcjs
import qualified "text"                 Data.Text as T
import           "this"                 Servant.Client.Ghcjs.Extended (callServant)
import           "transformers"         Control.Monad.Trans.State.Strict


--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

data Model = Model
  { _entryRecords        :: ![EntryRecord]
  , _pendingEntryRecords :: ![EntryRecord]
  , _field               :: !T.Text
  , _uid                 :: !Int
  , _visibility          :: !Visibility
  } deriving (Show, Generic, Eq)

data EntryRecord = EntryRecord
  { _entryRecEntry    :: !Entry
  , _entryRecEditing  :: !Bool
  , _entryRecFocussed :: !Bool
  } deriving (Show, Generic, Eq)

data Visibility = ViewAll | ViewActive | ViewCompleted
                  deriving (Show, Generic, Eq)

makeLenses ''Model
makeLenses ''EntryRecord

instance ToJSON EntryRecord
instance ToJSON Model
instance ToJSON Visibility

instance FromJSON EntryRecord
instance FromJSON Model
instance FromJSON Visibility

eid         :: Lens' EntryRecord EntryId
description :: Lens' EntryRecord T.Text
completed   :: Lens' EntryRecord Bool
eid         = entryRecEntry . entryId
description = entryRecEntry . entryEntry . entryInfDescription
completed   = entryRecEntry . entryEntry . entryInfCompleted


--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

viewModel :: Model -> View Action
viewModel m =
 div_
    [ class_ "todomvc-wrapper"
    , style_  $ M.singleton "visibility" "hidden"
    ]
    [ section_
        [ class_ "todoapp" ]
        [ viewInput m (m ^. field)
        , viewEntries (m ^. visibility) sortedEntries sortedPendingEntries
        , viewControls m (m ^. visibility) sortedEntries
        ]
    , infoFooter
    ]
  where
    sortedEntries, sortedPendingEntries :: [EntryRecord]
    sortedEntries        = sortEntries (m ^. entryRecords)
    sortedPendingEntries = sortEntries (m ^. pendingEntryRecords)

    sortEntries = sortBy (compare `on` L.view eid)

viewEntries :: Visibility -> [ EntryRecord ] -> [ EntryRecord ] -> View Action
viewEntries visibility entries pendingEntries =
  section_
    [ class_ "main"
    , style_ $ M.singleton "visibility" cssVisibility
    ]
    [ input_
        [ class_ "toggle-all"
        , type_ "checkbox"
        , name_ "toggle"
        , checked_ allCompleted
        , onClick $ CheckAll (not allCompleted)
        ] []
      , label_
        [ for_ "toggle-all" ]
          [ text $ S.pack "Mark all as complete" ]
      , ul_ [ class_ "todo-list" ] $
         map (viewEntry False) (filter isVisible entries) ++
         map (viewEntry True)  (filter isVisible pendingEntries)
      ]
  where
    cssVisibility = bool "visible" "hidden" (null entries)
    allCompleted = all (L.view completed) entries
    isVisible entryRec =
      case visibility of
        ViewCompleted ->       entryRec ^. completed
        ViewActive    -> not $ entryRec ^. completed
        _ -> True

viewEntry :: Bool -> EntryRecord -> View Action
viewEntry isPending entryRec = liKeyed_ (toKey $ entryRec ^. eid)
    [ class_ $ S.intercalate " " $
       [ "completed" | entryRec ^. completed ] <> [ "editing" | entryRec ^. entryRecEditing ]
    ]
    [ div_
        (if isPending then [class_ "disabled", disabled_ "true"] else [])
        [ div_
            [ class_ "view" ]
            [ input_
                [ class_ "toggle"
                , type_ "checkbox"
                , checked_ $ entryRec ^. completed
                , onClick $ Check (entryRec ^. eid) (not $ entryRec ^. completed)
                ] []
            , label_
                [ onDoubleClick $ EditingEntry (entryRec ^. eid) True ]
                [ text $ toMisoString $ entryRec ^. description ]
            , button_
                [ class_ "destroy"
                , onClick $ Delete (entryRec ^. eid)
                ] []
            ]
        , input_
            [ class_ "edit"
            , value_ (toMisoString $ entryRec ^. description)
            , name_ "title"
            , id_ $ "todo-" <> S.pack (show $ entryRec ^. eid)
            , onInput $ UpdateEntryDescription (entryRec ^. eid) . textFromJSString
            , onBlur  $ UpdateEntry entry
            , onEnter $ UpdateEntry entry
            ]
            []
        ]
    ]
  where
    entry = entryRec ^. entryRecEntry

viewControls :: Model ->  Visibility -> [ EntryRecord ] -> View Action
viewControls model visibility entries =
  footer_ [ class_ "footer"
          , hidden_ (bool "" "hidden" $ null entries)
          ]
      [ viewControlsCount entriesLeft
      , viewControlsFilters visibility
      , viewControlsClear model entriesCompleted
      ]
  where
    entriesCompleted = length . filter (L.view completed) $ entries
    entriesLeft = length entries - entriesCompleted

viewControlsCount :: Int -> View Action
viewControlsCount entriesLeft =
  span_ [ class_ "todo-count" ]
     [ strong_ [] [ text $ S.pack (show entriesLeft) ]
     , text (item_ <> " left")
     ]
  where
    item_ = S.pack $ bool " items" " item" (entriesLeft == 1)

viewControlsFilters :: Visibility -> View Action
viewControlsFilters visibility =
  ul_
    [ class_ "filters" ]
    [ visibilitySwap "#/" ViewAll visibility
    , text " "
    , visibilitySwap "#/active" ViewActive visibility
    , text " "
    , visibilitySwap "#/completed" ViewCompleted visibility
    ]

visibilitySwap :: MisoString -> Visibility -> Visibility -> View Action
visibilitySwap uri visibility actualVisibility =
  li_ [  ]
      [ a_ [ href_ uri
           , class_ $ S.concat [ "selected" | visibility == actualVisibility ]
           , onClick (ChangeVisibility visibility)
           ] [ viewVisibility visibility ]
      ]

viewVisibility :: Visibility -> View a
viewVisibility = text . \case
  ViewAll       -> "All"
  ViewActive    -> "Active"
  ViewCompleted -> "Completed"

viewControlsClear :: Model -> Int -> View Action
viewControlsClear _ entriesCompleted =
  button_
    [ class_ "clear-completed"
    , prop "hidden" (entriesCompleted == 0)
    , onClick DeleteComplete
    ]
    [ text $ "Clear completed (" <> S.pack (show entriesCompleted) <> ")" ]

viewInput :: Model -> T.Text -> View Action
viewInput _ task =
  header_ [ class_ "header" ]
    [ h1_ [] [ text "nixtodo" ]
    , input_
        [ class_ "new-todo"
        , placeholder_ "What needs to be done?"
        , autofocus_ True
        , value_ $ toMisoString task
        , name_ "newTodo"
        , onInput $ UpdateField . textFromJSString
        , onEnter Add
        ] []
    ]

onEnter :: Action -> Attribute Action
onEnter action =
  onKeyDown $ bool NoOp action . (== KeyCode 13)

infoFooter :: View a
infoFooter =
    footer_ [ class_ "info" ]
    [ p_ [] [ text "Double-click to edit a todo" ]
    , p_ []
        [ text "Written by "
        , a_ [ href_ "https://github.com/dmjio" ] [ text "David Johnson" ]
        ]
    , p_ []
        [ text "Adapted by "
        , a_ [ href_ "https://github.com/basvandijk" ] [ text "Bas van Dijk" ]
        ]
    , p_ []
        [ text "for the "
        , a_ [ href_ "https://github.com/basvandijk/nix-workshop" ]
             [ text "Nix Workshop @ Haskell eXchange 2017" ]
        ]
    , p_ []
        [ text "Part of "
        , a_ [ href_ "http://todomvc.com" ] [ text "TodoMVC" ]
        ]
    , p_ []
        [ text "Powered by "
        , icon "http://nixos.org"   "static/nix.png"
        , icon "http://haskell.org" "static/haskell.png"
        ]
    ]
  where
    icon url imgUrl =
      a_ [ href_ url ]
         [ img_ [ src_ imgUrl
                , width_ "32"
                , style_ (M.fromList [("vertical-align", "middle")])
                ]
                []
         ]


--------------------------------------------------------------------------------
-- Controller
--------------------------------------------------------------------------------

data Action
  = NoOp
  | Initialize
  | SetEntries ![Entry]
  | WebSocketEvent !(Miso.WebSocket EntryEvent) -- TODO
  | UpdateField !T.Text
  | EditingEntry !EntryId !Bool
  | UpdateEntryDescription !EntryId !T.Text
  | UpdateEntry !Entry
  | Add
  | AddEntryResult !EntryId !(Either ServantError Entry)
  | Delete !EntryId
  | DeleteComplete
  | Check !EntryId !Bool
  | CheckAll !Bool
  | ChangeVisibility !Visibility

updateModel :: Action -> Transition Action Model ()
updateModel = \case
    NoOp -> pure ()

    Initialize -> scheduleIO $ do
        result <- callServant "" $ readEntries nixtodoApiClient
        case result of
          Left err -> do
            print err
            threadDelay 1e6
            pure Initialize
          Right entries -> pure $ SetEntries entries

    SetEntries entries -> do
      entryRecords .=
        [ EntryRecord
          { _entryRecEntry    = entry
          , _entryRecEditing  = False
          , _entryRecFocussed = False
          }
        | entry <- entries
        ]

    WebSocketEvent webSocketAction ->
        case webSocketAction of
          WebSocketMessage entryEvent ->
              case entryEvent of
                UpsertEntryEvent entry -> do
                  let id' = entry ^. entryId
                  entryRecords %= filterMap ((== id') . L.view eid)
                    (entryRecEntry .~ entry)

                DeleteEntryEvent id' -> do
                  entryRecords %= filter ((/= id') . L.view eid)

          WebSocketClose _closeCode _wasClean _reason -> pure ()
          WebSocketOpen                               -> pure ()
          WebSocketError err                          -> pure ()

    Add -> do
      oldUid   <- use uid
      oldField <- use field

      uid   .= oldUid + 1
      field .= mempty

      unless (T.null oldField) $ do
        pendingEntryRecords %= (<> [newEntry oldField oldUid])

        scheduleIO $ do
          addEntryResult <- callServant "" $ createEntry nixtodoApiClient
            EntryInfo
            { _entryInfDescription = oldField
            , _entryInfCompleted   = False
            }
          pure $ AddEntryResult oldUid addEntryResult

    AddEntryResult id' addEntryResult -> do
        pendingEntryRecords %= filter ((/= id') . L.view eid)
        case addEntryResult of
          Left _err -> pure ()
          Right entry -> do
            entryRecords %=
              (<> [ EntryRecord
                    { _entryRecEntry    = entry
                    , _entryRecEditing  = False
                    , _entryRecFocussed = False
                    }
                  ]
              )

    UpdateField str -> field .= str

    EditingEntry id' isEditing -> do
      editingEntry id' isEditing

    UpdateEntryDescription id' desc -> do
      entryRecords %= filterMap ((== id') . L.view eid)
                   (description .~ desc)

    UpdateEntry entry -> do
      let id' = entry ^. entryId
      editingEntry id' False
      scheduleIO $ do
        _result <- callServant "" $
          updateEntry nixtodoApiClient id' $ entry ^. entryEntry
        pure NoOp -- TODO: handle errors

    Delete id' -> do
      entryRecords %= filter ((/= id') . L.view eid)

      scheduleIO $ do
        _result <- callServant "" $ deleteEntry nixtodoApiClient id'
        pure NoOp -- TODO: handle error

    DeleteComplete -> do
      entryRecs <- use entryRecords
      let (completedRecs, uncompletedRecs) = partition (L.view completed) entryRecs
      entryRecords .= uncompletedRecs

      scheduleIO $ do
        F.for_ completedRecs $ \completedRec -> do
          let id' = completedRec ^. entryRecEntry . entryId
          _result <- callServant "" $ deleteEntry nixtodoApiClient id'
          pure () -- TODO: handle errors
        pure NoOp

    Check id' isCompleted -> do
      entryRecs <- use entryRecords
      let (selectedRecs, otherRecs) = partition ((id' ==) . L.view eid) entryRecs

      F.for_ (listToMaybe selectedRecs) $ \selectedRec -> do
        let checkedRec = selectedRec & completed .~ isCompleted
        entryRecords .= checkedRec : otherRecs

        scheduleIO $ do
          _result <- callServant "" $ updateEntry nixtodoApiClient id'
            (checkedRec ^. entryRecEntry . entryEntry)
          pure NoOp -- TODO: handle error

    CheckAll isCompleted -> -- TODO
      entryRecords %= filterMap (const True)
                   (completed .~ isCompleted)

    ChangeVisibility v ->
      visibility .= v

editingEntry :: EntryId -> Bool -> Transition Action Model ()
editingEntry id' isEditing = do
    entryRecords %= filterMap ((== id') . L.view eid)
                 ( (entryRecEditing  .~ isEditing)
                 . (entryRecFocussed .~ isEditing)
                 )
    scheduleIO $ NoOp <$ focus ("todo-" <> S.pack (show id'))


newEntry :: T.Text -> Int -> EntryRecord
newEntry desc id' = EntryRecord
  { _entryRecEntry = Entry
      { _entryId = id'
      , _entryEntry = EntryInfo
          { _entryInfDescription = desc
          , _entryInfCompleted   = False
          }
      }
  , _entryRecEditing  = False
  , _entryRecFocussed = False
  }

filterMap :: (a -> Bool) -> (a -> a) -> [a] -> [a]
filterMap predicate f = go'
  where
    go' [] = []
    go' (y:ys)
     | predicate y = f y : go' ys
     | otherwise   =   y : go' ys


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    websocketUrl <- getWebsocketUrl
    startApp App
      { initialAction = Initialize
      , model  = emptyModel
      , view   = viewModel
      , update = fromTransition . updateModel
      , events = defaultEvents
      , subs = [websocketSub (URL websocketUrl) (Protocols []) WebSocketEvent]
      }
  where
    getWebsocketUrl :: IO MisoString
    getWebsocketUrl = do
        window   <- currentWindowUnchecked
        location <- getLocation window
        protocol <- getProtocol location
        host     <- getHostname location
        port     <- getPort location
        let wsProtocol = case protocol of
              "https:" -> "wss:"
              "http:"  -> "ws:"
              _ -> error $ "I don't know how to convert protocol: " <>
                           JSS.unpack protocol <>
                           " to a websocket protocol!"
        pure $ wsProtocol <> "//" <> host <> ":" <> port <> "/websocket"

emptyModel :: Model
emptyModel = Model
  { _entryRecords        = []
  , _pendingEntryRecords = []
  , _visibility          = ViewAll
  , _field               = mempty
  , _uid                 = 0
  }
