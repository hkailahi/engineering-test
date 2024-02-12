{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Processor.Execution where

import Control.Concurrent (threadDelay)
import Control.Lens (view)
import Data.Map.Strict qualified as Map
import Relude
import Settings (HasExecCfg (execCfgExecutionStore))
import Types
  ( AsyncExecution (AsyncExecution),
    ExecStatus (..),
    ExecutionStore (..),
    StepType (..),
    Task (taskId, taskSteps),
    TaskId,
  )
import UnliftIO.Async (async, cancel, link, poll)
import UnliftIO.STM (writeTMVar)

----------------------------------------------------------------------------------------------------
-- Initialize Execution Processing

newExecutionStore :: IO (TMVar ExecutionStore)
newExecutionStore = newTMVarIO $ ExecutionStore mempty

----------------------------------------------------------------------------------------------------
-- Request Execution

-- | Cancels an async execution with 'AsyncCancelled', a special async exception that gets ignored
-- by `link`. This means our parent worker thread doesn't get taken down alongside the
-- cancelled async execution.
cancelExecution :: TaskId -> TMVar ExecutionStore -> IO ExecStatus
cancelExecution tid executions = do
  (ExecutionStore store) <- atomically $ readTMVar executions
  case Map.lookup tid store of
    Nothing -> pure ExecNotFound
    Just (AsyncExecution runner) -> do
      cancel runner
      pure ExecCancelled

-- | Polls an async execution for it's current status
--
-- ```hs
-- -- | Check whether an 'Async' has completed yet.  If it has not
-- completed yet, then the result is @Nothing@, otherwise the result
-- is @Just e@ where @e@ is @Left x@ if the @Async@ raised an
-- exception @x@, or @Right a@ if it returned a value @a@.
--
-- poll :: Async a -> IO (Maybe (Either SomeException a))
-- ```
checkExecutionStatus :: TaskId -> TMVar ExecutionStore -> IO ExecStatus
checkExecutionStatus tid executions = do
  (ExecutionStore store) <- atomically $ readTMVar executions
  case Map.lookup tid store of
    Nothing -> pure ExecNotFound
    Just (AsyncExecution runner) -> do
      poll runner >>= \case
        Nothing -> pure ExecInProgress
        Just (Left _err) -> pure ExecCancelled
        Just (Right _) -> pure ExecCompleted

----------------------------------------------------------------------------------------------------
-- Task Execution

execStep :: StepType -> IO ()
execStep = \case
  NoOp -> pure ()
  SleepShort -> threadDelay 1_000_000
  SleepMid -> threadDelay 3_000_000
  SleepLong -> threadDelay 5_000_000

execTask :: Task -> IO ()
execTask = traverse_ execStep . taskSteps

recordExecution :: Task -> AsyncExecution -> TMVar ExecutionStore -> IO ()
recordExecution task action store = do
  executions <- atomically $ readTMVar store
  atomically
    . writeTMVar store
    $! ExecutionStore
    . Map.insert (taskId task) action
    $ unExecutionStore executions

invokeAsyncExecution :: (MonadReader r m, MonadIO m, HasExecCfg r) => Task -> m ()
invokeAsyncExecution task = do
  executions <- view execCfgExecutionStore
  runner <- liftIO . async $ do
    execTask task
    putStrLn $ "Task Completed: " <> show task <> "\n"
  link runner
  liftIO $ recordExecution task (AsyncExecution runner) executions

-- -- FIXME Replace `withAsync` over raw `async` in `invokeAsyncExecution`. I ran out of time to
-- --  embed `loop` in `withAsync` calls and pass the type-checker. The commented block below
-- -- type-checks but executes synchronously.
--
-- invokeWronglySynchronousExecution :: (MonadReader r m, MonadIO m, HasExecCfg r) => Task -> m ExecStatus
-- invokeWronglySynchronousExecution task = do
--   executions <- view execCfgExecutionStore
--   liftIO $ withAsync (execTask task) \runner -> do
--     void . liftIO $ recordExecution task (AsyncExecution runner) executions
--     waitCatch runner >>= \case
--       Left _ -> do
--         putStrLn $ "Task Cancelled: " <> show task <> "\n"
--         pure ExecCancelled
--       Right _ -> do
--         putStrLn $ "Task Completed: " <> show task <> "\n"
--         pure ExecCompleted
