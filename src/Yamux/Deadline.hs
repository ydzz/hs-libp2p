{-#language RecordWildCards#-}
module Yamux.Deadline (
   mkPipeDeadline,
   setDeadline,
   TimerCommand(..),
   closeDeadline,
   waitDeadline
   ) where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.Async
import Data.Functor
import Data.Maybe
import qualified Data.Time as T

data TimerCommand = TimerReset Int | TimeClose deriving (Show, Eq)

data PipeDeadline = PipeDeadline {
   lockVar::TMVar (),
   cancel::TMVar (),
   timerControl :: TBQueue TimerCommand,
   runId::TVar (Maybe ThreadId)
}

mkPipeDeadline::IO PipeDeadline
mkPipeDeadline = atomically $ do
   lockVar <- newTMVar ()
   cancel <- newEmptyTMVar
   timerControl <- newTBQueue 1
   runId <- newTVar Nothing
   pure PipeDeadline {..}

setDeadline:: PipeDeadline -> T.UTCTime -> IO ()
setDeadline deadLine@PipeDeadline{..} time = do
   atomically $ do
      takeTMVar lockVar
      tryTakeTMVar cancel
   mayTid <- readTVarIO runId
   now <- T.getCurrentTime
   let diff = T.diffUTCTime time now
   let delayTime = floor $ toRational diff  * 1000000
   if delayTime > 0
      then
         case mayTid of
            Nothing -> do
             tid <- forkIO (startloop  delayTime)
             atomically $ writeTVar runId $ Just tid
            Just _ -> atomically $ writeTBQueue timerControl (TimerReset delayTime)
      else closeDeadline deadLine
   atomically $ putTMVar lockVar ()
 where
   startloop::Int -> IO ()
   startloop delay = do
    e <- race (threadDelay delay) (atomically $ readTBQueue timerControl)
    case e of
      Left _    -> atomically $ do
                               putTMVar cancel ()
                               writeTVar runId Nothing
      Right cmd ->
         case cmd of
            TimeClose    -> atomically $ do
                             tryPutTMVar cancel ()
                             writeTVar runId Nothing
            TimerReset t -> startloop t


closeDeadline::PipeDeadline -> IO ()
closeDeadline PipeDeadline{..} = atomically $ writeTBQueue timerControl TimeClose

waitDeadline::PipeDeadline -> IO ()
waitDeadline PipeDeadline{..} = atomically $ readTMVar cancel