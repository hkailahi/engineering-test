{-# LANGUAGE NoImplicitPrelude #-}

module Processor.Task where

import Control.Concurrent.STM.TBMQueue (TBMQueue, newTBMQueueIO)
import Relude
import Types (Task)

----------------------------------------------------------------------------------------------------
-- Initialize Task Processing

newTaskQueue :: Int -> IO (TBMQueue Task)
newTaskQueue = newTBMQueueIO
