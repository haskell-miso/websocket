-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE CPP                        #-}
-----------------------------------------------------------------------------
module WebSocket (websocketComponent) where
-----------------------------------------------------------------------------
import           Control.Monad (unless)
import           GHC.Generics
-----------------------------------------------------------------------------
import           Miso hiding (on)
import           Miso.Html
import           Miso.Html.Property
import           Miso.Lens
import           Miso.WebSocket
import           Miso.String (ToMisoString)
import qualified Miso.String as MS
-----------------------------------------------------------------------------
data Message
  = Message
  { dateString :: MisoString
  , message :: MisoString
  , origin :: Origin
  } deriving (Eq, Show, Generic)
-----------------------------------------------------------------------------
data Origin = CLIENT | SYSTEM | SERVER
  deriving (Eq, Show, Generic)
-----------------------------------------------------------------------------
instance ToMisoString Origin where
  toMisoString = \case
    CLIENT -> "CLIENT"
    SYSTEM -> "SYSTEM"
    SERVER -> "SERVER"
-----------------------------------------------------------------------------
data Action
  = OnOpen WebSocket
  | OnMessage MisoString
  | OnClosed Closed
  | OnError MisoString
  | Send
  | SendMessage MisoString
  | Update MisoString
  | Append Message
  | Connect
  | Disconnect
  | NoOp
  | CloseBox
  | Clear
-----------------------------------------------------------------------------
data Model = Model
  { _msg :: MisoString
  , _received :: [Message]
  , _websocket :: WebSocket
  , _connected :: Bool
  , _connections :: [WebSocket]
  , _clearInput :: Bool
  , _boxId :: Int
  } deriving Eq
-----------------------------------------------------------------------------
msg :: Lens Model MisoString
msg = lens _msg $ \r x -> r { _msg = x }
-----------------------------------------------------------------------------
received :: Lens Model [Message]
received = lens _received $ \r x -> r { _received = x }
-----------------------------------------------------------------------------
websocket :: Lens Model WebSocket
websocket = lens _websocket $ \r x -> r { _websocket = x }
-----------------------------------------------------------------------------
connected :: Lens Model Bool
connected = lens _connected $ \r x -> r { _connected = x }
-----------------------------------------------------------------------------
clearInput :: Lens Model Bool
clearInput = lens _clearInput $ \r x -> r { _clearInput = x }
-----------------------------------------------------------------------------
boxId :: Lens Model Int
boxId = lens _boxId $ \r x -> r { _boxId = x }
-----------------------------------------------------------------------------
emptyModel :: Int -> Model
emptyModel = Model mempty [] emptyWebSocket False [] True
-----------------------------------------------------------------------------
websocketComponent :: Int -> Component parent props Model Action
websocketComponent box = component (emptyModel box) updateModel viewModel
  where
    updateModel = \case
      Send -> do
        m <- use msg
        unless (MS.null m) $ do
          issue (SendMessage m)
          clearInput .= True
          msg .= ""
          io $ do
            date <- newDate
            dateString <- date & toLocaleString
            pure $ Append (Message dateString m CLIENT)
      SendMessage m -> do
        socket <- use websocket
        sendText socket m
      Connect ->
        connectText
          "wss://echo.websocket.org"
          OnOpen
          OnClosed
          OnMessage
          OnError
      OnOpen socket -> do
        websocket .= socket
        connected .= True
      OnClosed closed -> do
        connected .= False
        io $ do
          date <- newDate
          dateString <- date & toLocaleString
          consoleLog $ ms (show closed)
          pure $ Append (Message dateString "Disconnected..." SYSTEM)
      OnMessage message ->
        io $ do
          date <- newDate
          dateString <- date & toLocaleString
          pure $ Append (Message dateString message SERVER)
      Append message ->
        received %= (message :)
      OnError errorMessage ->
        io_ (consoleError errorMessage)
      Update input -> do
        clearInput .= False
        msg .= input
      NoOp ->
        pure ()
      CloseBox ->
        broadcast box
      Disconnect ->
        close =<< use websocket
      Clear -> do
        clearInput .= True
        msg .= ""
        received .= []
-----------------------------------------------------------------------------
viewModel :: props -> Model -> View Model Action
viewModel _ m =
  div_
  [ className "websocket-box" ]
  [ div_
    [ class_ "websocket-header" ]
    [ vfrag
      [ span_
        [ classList_
          [ ("websocket-status", True)
          , ("status-disconnected", not (m ^. connected))
          , ("status-connected", m ^. connected)
          ]
        ]
        []
      , span_
        [ class_ "websocket-id"
        ]
        [ text $ "socket-" <> ms (m ^. boxId) ]
      ]
    , button_
      [ aria_ "label" "Close"
      , class_ "btn-close"
      , onClick CloseBox
      ]
      [ "×" ]
    ]
    , div_
      [ class_ "websocket-controls" ]
      [ optionalAttrs
        button_
        [ class_ "btn btn-success connect-btn"
        , onClick Connect
        ]
        (m ^. connected)
        [ disabled_ ]
        [ "Connect" ]
      , optionalAttrs
        button_
        [ class_ "btn btn-danger disconnect-btn"
        , onClick Disconnect
        ]
        (not (m ^. connected))
        [ disabled_ ]
        ["Disconnect"]
      , optionalAttrs
        button_
        [ class_ "btn btn-primary"
        , onClick Clear
        ]
        (null (m ^. received))
        [ disabled_ ]
        ["Clear"]
      ]
    , div_
      [ class_ "websocket-input" ]
      [ input_ $
        [ placeholder_ "Type a message..."
        , class_ "input-field message-input"
        , onInput Update
        , onEnter NoOp Send
        , type_ "text"
        ] ++
        [ disabled_
        | not (m ^. connected)
        ] ++
        [ value_ ""
        | m ^. clearInput
        ]
      , optionalAttrs
        button_
        [ class_ "btn btn-primary send-btn"
        , onClick Send
        ]
        (not (m ^. connected))
        [ disabled_ ]
        [ "Send"
        ]
      ]
    , div_
      [ class_ "messages-list"
      ] $
      if null (m ^. received)
      then
        pure $ div_
          [ class_ "empty-state"
          ]
          [ "No messages yet"
          ]
      else messageHeader (m ^. received)
    ]
-----------------------------------------------------------------------------
messageHeader :: [Message] -> [View model action]
messageHeader messages = concat
  [ 
    [ div_
      [ class_ "message-header" ]
      [ span_ [class_ "message-origin"] [ text (ms origin) ]
      , span_ [class_ "timestamp"] [ text dateString ]
      ]
    , div_ [class_ "message-content"] [ text message ]
    ]
  | Message dateString message origin <- messages
  ]
-----------------------------------------------------------------------------
