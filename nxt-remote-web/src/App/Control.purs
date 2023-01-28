module App.Control where

import Prelude

import Control.Comonad (extract)
import Control.Monad.Except (Except, runExceptT)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String.Read (class Read, read)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Foreign (MultipleErrors, readString)
import Halogen (ClassName(..), Component, ComponentHTML, HalogenM, defaultEval, get, liftEffect, mkComponent, mkEval, modify_, subscribe) as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onMouseDown, onMouseUp, onTouchEnd, onTouchStart) as H
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Web.Event.Event (Event, EventType(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Location (hostname)
import Web.HTML.Window (document, location)
import Web.Socket.Event.MessageEvent as WSE
import Web.Socket.WebSocket (Protocol(..), WebSocket)
import Web.Socket.WebSocket as WS
import Web.UIEvent.KeyboardEvent as KE

type State = { currentAction :: Action, ws :: Maybe WebSocket, frontDistance :: Maybe Int }

data Action = Init | North | South | West | East | StopDriving | StopSteering | Terminate | WebSocketEvent String

derive instance eq :: Eq Action

instance Read Action where
  read "w" = Just North
  read "s" = Just South
  read "d" = Just East
  read "a" = Just West
  read "t" = Just StopDriving
  read "f" = Just StopSteering
  read "x" = Just Terminate
  read _ = Nothing

instance Show Action where
  show North = "North" 
  show South = "South"
  show West = "West"
  show East = "East"
  show StopDriving = "StopDriving"
  show StopSteering = "StopSteering"
  show Terminate = "Terminate"
  show Init = "Init"
  show _ = ""

component :: forall q i o m. MonadEffect m => MonadAff m =>  H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { currentAction: Init, ws : Nothing, frontDistance : Nothing }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div [HP.classes [HH.ClassName "container"]] [
          HH.p [ HP.classes [HH.ClassName "text" ]] [ 
          case state.frontDistance of 
             (Just (-1)) -> HH.text $ "The current frontdistance is unknown." 
             (Just n) -> HH.text $ "The current frontdistance is: " <> show n <> " centimeters." 
             Nothing -> HH.text $ "The current frontdistance is unknown." 
            ],
      HH.div [ HP.classes [HH.ClassName "contanier__first-row"] ] [
        HH.button [HP.classes [HH.ClassName "btn btn-secondary btn-up"]] [ HH.i [ HP.classes [HH.ClassName "fa fa-arrow-up"], H.onMouseDown (\_ -> North), H.onMouseUp (\_ -> StopDriving),
        H.onTouchStart (\_ -> North), H.onTouchEnd (\_ -> StopDriving) 
          ] [ ] ]
      ],
      HH.div [ HP.classes [HH.ClassName "contanier__second-row" ]] [ 
         HH.button [HP.classes [HH.ClassName "btn btn-secondary"]] [HH.i [ HP.classes [HH.ClassName "fa fa-arrow-left"], H.onMouseDown (\_ -> West), H.onMouseUp (\_ -> StopSteering), 
      H.onTouchStart (\_ -> West), H.onTouchEnd (\_ -> StopSteering)] []], 

      HH.button [HP.classes [HH.ClassName "btn btn-secondary"]] [HH.i [ HP.classes [HH.ClassName "fa fa-arrow-down"], H.onMouseDown (\_ -> South), H.onMouseUp (\_ -> StopDriving),
      H.onTouchStart (\_ -> South), H.onTouchEnd (\_ -> StopDriving)] [ ] ],
      HH.button [HP.classes [HH.ClassName "btn btn-secondary"]] [ HH.i [ HP.classes [HH.ClassName "fa fa-arrow-right"], H.onMouseDown (\_ -> East), H.onMouseUp (\_ -> StopSteering),
      H.onTouchStart (\_ -> East), H.onTouchEnd (\_ -> StopSteering)] []]]
    ]

handleAction :: forall cs o m. MonadEffect m => MonadAff m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Init -> do  
   loc <- H.liftEffect $ hostname =<< location =<< window

   websocket <- H.liftEffect $ WS.create ("ws://" <> loc <>  ":8080/stream") [Protocol "ws"]

   H.modify_ $ \state -> state { ws = Just websocket }

   doc <- H.liftEffect $ toEventTarget <$> (document =<< window)

   let keyDownEmitter = eventListener (EventType "keydown") doc (\e -> (KE.key <$> KE.fromEvent e) >>= read)
   let keyUpEmitter = eventListener (EventType "keyup") doc handleKeyupEvent
   let websocketEmitter = eventListener (EventType "message") (WS.toEventTarget websocket) handleWebSocketEvent

   void $ H.subscribe keyUpEmitter
   void $ H.subscribe keyDownEmitter  
   void $ H.subscribe websocketEmitter

   pure unit
  WebSocketEvent string -> do
      H.liftEffect $ log string
      H.modify_ $ \state -> state { frontDistance = fromString string}
  action -> do 
   {currentAction, ws} <- H.get
   unless (currentAction == action) $ do
                  H.modify_ $ \state -> state {currentAction= action}
                  case ws of 
                     (Just websocket) -> H.liftEffect $ WS.sendString websocket (show action)
                     Nothing -> pure unit
                  pure unit
   H.liftEffect $ log $ show action

handleWebSocketEvent :: Event -> Maybe Action
handleWebSocketEvent e = let event = WSE.fromEvent e
                             s = case event of 
                                   (Just ev) -> readString $ (WSE.data_ ev) :: Except MultipleErrors String 
                                   Nothing -> pure "-1"
                             result = case (extract $ runExceptT  s) of 
                                         (Right value) -> value
                                         (Left _) -> "-1"
                         in Just $ WebSocketEvent $ result

handleKeyupEvent ∷ Event → Maybe Action
handleKeyupEvent e = do
       let key = (KE.key <$> KE.fromEvent e) >>= read
       case key of
          Just "w" -> Just StopDriving
          Just "s" -> Just StopDriving 
          Just "a" -> Just StopSteering
          Just "d" -> Just StopSteering
          _ -> Nothing