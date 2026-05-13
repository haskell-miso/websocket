-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE CPP                        #-}
-----------------------------------------------------------------------------
module Main (main) where
-----------------------------------------------------------------------------
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
-----------------------------------------------------------------------------
import           Miso hiding (on)
import           Miso.Html hiding (title_)
import           Miso.Html.Property
import           Miso.Lens
-----------------------------------------------------------------------------
import           WebSocket
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
data Action
  = AddWebSocket
  | Close Int
  | NoOp
-----------------------------------------------------------------------------
data Model = Model
  { _nextConnection :: Int
  , _connections :: IntSet
  } deriving Eq
-----------------------------------------------------------------------------
nextConnection :: Lens Model Int
nextConnection = lens _nextConnection $ \r x -> r { _nextConnection = x }
-----------------------------------------------------------------------------
connections :: Lens Model IntSet
connections = lens _connections $ \r x -> r { _connections = x }
-----------------------------------------------------------------------------
main :: IO ()
main = startApp (keyboardEvents <> defaultEvents) app
-----------------------------------------------------------------------------
app :: App Model Action
app = (component emptyModel update_ appView)
  { mailbox = checkMail Close (const NoOp)
#ifndef WASM
  , styles = [ Href "assets/style.css" ]
#endif
  } where
     emptyModel = Model 0 mempty
     update_ (Close x) =
       connections %= IS.delete x
     update_ AddWebSocket = do
       nextConnection += 1
       connId <- use nextConnection
       connections %= IS.insert connId
     update_ NoOp =
       pure ()
-----------------------------------------------------------------------------
githubStar :: View model action
githubStar = iframe_
    [ title_ "GitHub"
    , height_ "30"
    , width_ "170"
    , textProp "scrolling" "0"
    , textProp "frameborder" "0"
    , src_
      "https://ghbtns.com/github-btn.html?user=haskell-miso&repo=miso-websocket&type=star&count=true&size=large"
    ]
    []
-----------------------------------------------------------------------------
appView :: Model -> View Model Action
appView m = vfrag
  [ githubStar
  , div_
    [ class_ "container"
    ]
    [ h1_
      []
      [ "🍜 "
      , a_
        [ href_ "https://github.com/haskell-miso/miso-websocket" ]
        [ "miso-websocket"
        ]
      , " ⚡" 
      ]
    , div_
      [ class_ "controls" ]
      [ button_
        [ class_ "btn btn-primary"
        , id_ "add-websocket-btn"
        , onClick AddWebSocket
        ]
        [ "Add New WebSocket"
        ]
      ]
    , div_
      [ class_ "websockets-container"
      , id_ "websockets-container"
      ] -- the syncChildren case should kick in here as well
      [ div_ [ key_ connId ] [ mount_ (websocketComponent connId) ]
      | connId <- IS.toList (m ^. connections)
      ]
    ]
  ]
-----------------------------------------------------------------------------
