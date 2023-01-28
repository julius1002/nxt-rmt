{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module App where

import Control
import Control.Concurrent
import Control.Concurrent.Lifted
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import qualified Network.WebSockets as WS
import Network.WebSockets.Connection (PendingConnection, acceptRequest)
import Robotics.NXT (getBatteryLevel)
import qualified Robotics.NXT as EXT
import Robotics.NXT.Sensor.Ultrasonic
import Servant
import Servant.API
import Servant.API.WebSocket
import System.Console.GetOpt
import System.Environment
import System.Exit
import qualified System.Hardware.Serialport as S
import System.IO
import qualified Text.Read as TIO

type HttpAPI = "move" :> QueryParam "direction" Move :> Get '[JSON] ()

data Move = North | South | West | East | StopDriving | StopSteering | Terminate deriving (Read)

instance FromHttpApiData Move where
  parseQueryParam text =
    let maybeMove = (TIO.readMaybe . T.unpack) text :: Maybe Move
     in case maybeMove of
          (Just move) -> Right move
          Nothing -> Left "direction could not be read"

moveApiHandler :: MonadIO f => EXT.NXTInternals -> Maybe Move -> f ()
moveApiHandler nxtInternals direction = void $
  liftIO $ do
    case direction of
      Just North -> EXT.execNXT forward nxtInternals
      Just South -> EXT.execNXT backward nxtInternals
      Just West -> EXT.execNXT left nxtInternals
      Just East -> EXT.execNXT right nxtInternals
      Just StopDriving -> EXT.execNXT stop nxtInternals
      Just StopSteering -> EXT.execNXT stopSteeringWheel nxtInternals
      Just Terminate -> EXT.terminate nxtInternals >> pure nxtInternals
      Nothing -> pure nxtInternals

moveApi :: Proxy HttpAPI
moveApi = Proxy

-- websocket
-- does not work in chrome with plain ws protocol

moveEventHandler :: MonadIO m => MVar (Maybe EXT.Measurement) -> EXT.NXTInternals -> PendingConnection -> m ()
moveEventHandler ultrasonicMeasurement nxtInternals pending = do
  conn <- liftIO $ acceptRequest pending
  liftIO $
    forkIO $
      forever $
        do
          measurement <- Control.Concurrent.Lifted.takeMVar ultrasonicMeasurement
          print measurement
          print "took mvar"
          case measurement of
            (Just measurement) -> WS.sendTextData conn $ T.pack $ show measurement
            Nothing -> pure ()
  liftIO $
    WS.withPingThread conn 30 (return ()) $ do
      forever $ do
        msg <- T.unpack <$> (WS.receiveData conn :: IO T.Text)
        moveApiHandler nxtInternals (Just (read msg))
        pure ()

type WebSocketAPI = "stream" :> WebSocketPending

myAPI :: Proxy WebSocketAPI
myAPI = Proxy

websocket :: MVar (Maybe EXT.Measurement) -> Reader EXT.NXTInternals (Server WebSocketAPI)
websocket ultrasonicMeasurement = asks (moveEventHandler ultrasonicMeasurement)

httpInterface :: Reader EXT.NXTInternals (Server HttpAPI)
httpInterface = asks moveApiHandler

type API = WebSocketAPI :<|> HttpAPI

api :: Proxy API
api = Proxy

app1 :: MVar (Maybe EXT.Measurement) -> Reader EXT.NXTInternals Application
app1 ultrasonicMeasurement = do
  ws <- websocket ultrasonicMeasurement
  http <- httpInterface
  pure $ cors (const $ Just corsP) (serve api $ ws :<|> http)
  where
    corsP =
      simpleCorsResourcePolicy
        { corsOrigins = Just (["http://localhost:1234", "http://192.168.1.100:1234"], True),
          corsMethods = ["OPTIONS", "GET", "PUT", "POST", "DELETE"],
          corsRequestHeaders = ["Authorization", "Content-Type"]
        }

main :: IO ()
main = do
  ultrasonicMeasurement <- Control.Concurrent.Lifted.newMVar Nothing
  nxtInternals <- initNXT "/dev/tty.NXT" ultrasonicMeasurement
  run 8080 (runReader (app1 ultrasonicMeasurement) nxtInternals)