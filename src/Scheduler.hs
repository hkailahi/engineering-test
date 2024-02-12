{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Scheduler where

import Control.Concurrent.STM.TBMQueue (closeTBMQueue, isClosedTBMQueue, readTBMQueue)
import Control.Lens (view)
import Processor.Execution (invokeAsyncExecution)
import Processor.Request
  ( getRequest,
    handleRequest,
  )
import Relude
import Settings
  ( HasExecCfg (execCfgTaskQueue),
  )
import Types
  ( DoOrDie (Run, Shutdown),
  )

----------------------------------------------------------------------------------------------------
-- Worker-Scheduler Threads

-- | Responds to user requests from driver.
--
-- Recieves requests, executes unqueueable request types, and populates task queue.
requestTasks :: (MonadReader r m, MonadIO m, HasExecCfg r) => m ()
requestTasks = do
  queue <- view execCfgTaskQueue

  fix $ \loop ->
    liftIO getRequest >>= \case
      Shutdown -> do
        atomically $ closeTBMQueue queue -- NOTE : Signals `doTasks` to complete on its next iteration
      Run req -> do
        handleRequest req
        loop

-- | Executes tasks.
--
--  Consumes task queue, invokes executions, and tracks executions by populating the execution store
doTasks :: (MonadReader r m, MonadIO m, HasExecCfg r) => m ()
doTasks = do
  queue <- view execCfgTaskQueue

  fix $ \loop -> do
    unlessM (atomically $ isClosedTBMQueue queue) do
      atomically (readTBMQueue queue) >>= \case
        Nothing -> do
          loop
        Just task -> do
          _completionStatus <- invokeAsyncExecution task
          loop
