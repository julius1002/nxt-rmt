module Control where

import Control.Concurrent
import Control.Concurrent.Lifted
import Control.Monad.Cont
import Robotics.NXT (getBatteryLevel)
import qualified Robotics.NXT as EXT
import Robotics.NXT.Sensor.Ultrasonic

left :: EXT.NXT ()
left = do
  EXT.setOutputState EXT.C 0 [EXT.MotorOn] EXT.RegulationModeIdle 0 EXT.MotorRunStateRunning 0
  EXT.setOutputState EXT.C (-60) [EXT.MotorOn] EXT.RegulationModeIdle 0 EXT.MotorRunStateRunning 0

right :: EXT.NXT ()
right = do
  EXT.setOutputState EXT.C 0 [EXT.MotorOn] EXT.RegulationModeIdle 0 EXT.MotorRunStateRunning 0
  EXT.setOutputState EXT.C 60 [EXT.MotorOn] EXT.RegulationModeIdle 0 EXT.MotorRunStateRunning 0

backward :: EXT.NXT ()
backward = do
  EXT.setOutputState EXT.A 80 [EXT.MotorOn] EXT.RegulationModeIdle 0 EXT.MotorRunStateRunning 0
  EXT.setOutputState EXT.B 80 [EXT.MotorOn] EXT.RegulationModeIdle 0 EXT.MotorRunStateRunning 0

forward :: EXT.NXT ()
forward = do
  EXT.setOutputState EXT.A (-80) [EXT.MotorOn] EXT.RegulationModeIdle 0 EXT.MotorRunStateRunning 0
  EXT.setOutputState EXT.B (-80) [EXT.MotorOn] EXT.RegulationModeIdle 0 EXT.MotorRunStateRunning 0

stop :: EXT.NXT ()
stop = do
  EXT.setOutputState EXT.A 0 [EXT.MotorOn] EXT.RegulationModeIdle 0 EXT.MotorRunStateRunning 0
  EXT.setOutputState EXT.B 0 [EXT.MotorOn] EXT.RegulationModeIdle 0 EXT.MotorRunStateRunning 0

readTouchSensor :: EXT.NXT EXT.InputValue
readTouchSensor = EXT.getInputValues EXT.One

initTouchSensor :: EXT.NXT ()
initTouchSensor = EXT.setInputMode EXT.One EXT.Switch EXT.BooleanMode

initUltraSonic :: EXT.NXT ()
initUltraSonic = usInit EXT.Four

readFrontDistance :: EXT.NXT (Maybe EXT.Measurement)
readFrontDistance = usGetMeasurement EXT.Four 0

stopSteeringWheel :: EXT.NXT ()
stopSteeringWheel = EXT.setOutputState EXT.C 0 [EXT.MotorOn] EXT.RegulationModeIdle 0 EXT.MotorRunStateRunning 0

activateUltraSonicStoppage :: MVar (Maybe EXT.Measurement) -> EXT.NXT ThreadId
activateUltraSonicStoppage measurement = do
  fork $ do
    forever $ do
      liftIO $ Control.Concurrent.Lifted.threadDelay 300000 -- update ultrasonic every 0.3 seconds
      value <- readFrontDistance
      Control.Concurrent.Lifted.putMVar measurement value

-- case value of -- use this for automatic stoppage
--   (Just n) -> do
--     when (n < 30) $ stop
--   Nothing -> pure ()

printBatteryLevel :: EXT.NXTInternals -> IO EXT.NXTInternals
printBatteryLevel = EXT.execNXT (getBatteryLevel >>= (\blevel -> liftIO $ print $ "current battery status: " <> show blevel))

activateUltraSonic :: EXT.NXTInternals -> IO EXT.NXTInternals
activateUltraSonic = EXT.execNXT initUltraSonic

activateUltraSonicAutomaticStoppage :: MVar (Maybe EXT.Measurement) -> EXT.NXTInternals -> IO EXT.NXTInternals
activateUltraSonicAutomaticStoppage measurement = EXT.execNXT $ activateUltraSonicStoppage measurement

type Device = String

initNXT :: Device -> MVar (Maybe EXT.Measurement) -> IO EXT.NXTInternals
initNXT device ultrasonicMeasurement = do
  let device = "/dev/tty.NXT"
  nxtInternals <- EXT.initialize device
  printBatteryLevel nxtInternals

  activateUltraSonic nxtInternals
  activateUltraSonicAutomaticStoppage ultrasonicMeasurement nxtInternals

  pure nxtInternals