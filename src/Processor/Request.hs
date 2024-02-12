{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Processor.Request where

import Control.Concurrent.STM.TBMQueue (writeTBMQueue)
import Control.Lens (view)
import Data.Map.Strict qualified as Map
import Data.Text (unpack)
import Data.UUID (UUID)
import Processor.Execution
  ( cancelExecution,
    checkExecutionStatus,
  )
import Relude
import Settings (HasExecCfg (execCfgExecutionStore, execCfgTaskQueue))
import Types
  ( DoOrDie (Run, Shutdown),
    DriverMessage (..),
    ExecStatus (..),
    RequestType (..),
    Task (..),
    TaskConfig (DefaultTaskConfig),
    TaskId (TaskId, unTaskId),
    TaskName (TaskName),
    mkTaskId,
    taskLibrary,
  )

----------------------------------------------------------------------------------------------------
-- Request Sourcers and Parsers

getRequest :: IO DoOrDie
getRequest = do
  msg <- DriverMessage <$> getLine -- TODO Improve `getLine`
  req <- parseRequest msg
  putStrLn $ "\nRequest received: " <> show req
  pure req

-- TODO - Add new request to populate task library with user-defined tasks or handle elsewhere.
parseRequest :: DriverMessage -> IO DoOrDie
parseRequest (DriverMessage msg) =
  case words msg of
    ["exit"] -> pure Shutdown
    ["help"] -> pure $ Run RequestHelp
    ["start", taskName] -> parseStart taskName Nothing
    ["start", taskName, opts] | opts == "default" -> parseStart taskName $ Just opts
    ["cancel", taskId] -> pure $ parseActionOnTask taskId True
    ["check", taskId] -> pure $ parseActionOnTask taskId False
    _ -> do pure $ Run RequestInvalid
  where
    -- NOTE: Currently task configurations are ignored, but we could assign them below
    parseStart :: Text -> Maybe Text -> IO DoOrDie
    parseStart taskName _opts =
      case Map.lookup (TaskName taskName) taskLibrary of
        Nothing -> pure $ Run RequestInvalid
        Just steps ->
          let currTask = Task <$> mkTaskId <*> pure DefaultTaskConfig <*> pure steps
           in Run . RequestValid <$> currTask
    parseActionOnTask :: Text -> Bool -> DoOrDie
    parseActionOnTask taskId isCancelElseStatus =
      let mbTaskId = fmap TaskId . readMaybe @UUID $ unpack taskId
          -- FIXME: Brittle, boolean-blind hack for minor reuse
          desiredReq :: TaskId -> RequestType
          desiredReq =
            if isCancelElseStatus
              then RequestCancel
              else RequestStatusCheck
       in Run $ maybe RequestInvalid desiredReq mbTaskId

----------------------------------------------------------------------------------------------------
-- Request Handling / Execution

helperMsg :: Text
helperMsg =
  "Enter a request. For example, try one of the following: "
    <> "\n* start uw"
    <> "\n* start squirrel"
    <> "\n* start bubbleBathOptimization"
    <> "\n* check <some-task-uuid>"
    <> "\n* cancel <some-task-uuid>"
    <> "\n* exit\n"

handleRequest :: (MonadReader r m, MonadIO m, HasExecCfg r) => RequestType -> m ()
handleRequest = \case
  RequestHelp -> do
    putStrLn $ unpack helperMsg
  RequestInvalid -> do
    putStrLn "Response: Invalid Request\n"
    putStrLn $ unpack helperMsg
  RequestValid task -> do
    queue <- view execCfgTaskQueue
    atomically . writeTBMQueue queue $! task
    putStrLn
      $ "Request: Task "
      <> show (unTaskId $ taskId task)
      <> " queued.\n"
  RequestCancel tid -> do
    store <- view execCfgExecutionStore
    liftIO (cancelExecution tid store)
      >>= \case
        -- FIXME: Edge cases where:
        -- \* Cancellation fails
        --    * Tasks completes before `cancel` runs
        --    * Or `cancel` itself errors before `ExecCancelled`)
        -- \* Task is not found because it hasn't reached queue yet (unlikely)
        -- \* Task is not found because it hasn't been recorded in execution store yet (more likely)
        ExecCancelled ->
          putStrLn
            $ "Cancellation: Task "
            <> show (unTaskId tid)
            <> " cancelled.\n"
        ExecNotFound ->
          putStrLn
            $ "Cancellation Response: Task "
            <> show (unTaskId tid)
            <> " not found. "
            <> "The task may not have been started yet.\n"
        _ ->
          putStrLn
            $ "Cancellation Response: Impossible state. Task "
            <> show (unTaskId tid)
            <> " cancellation ignored.\n"
  RequestStatusCheck tid -> do
    store <- view execCfgExecutionStore
    liftIO (checkExecutionStatus tid store) >>= \case
      ExecCancelled ->
        putStrLn
          $ "Status Response: Task "
          <> show (unTaskId tid)
          <> " has been cancelled.\n"
      ExecCompleted ->
        putStrLn
          $ "Status Response: Task "
          <> show (unTaskId tid)
          <> " has been completed.\n"
      ExecInProgress ->
        putStrLn
          $ "Status Response: Task "
          <> show (unTaskId tid)
          <> " is in progress.\n"
      -- FIXME: Edge cases where:
      -- \* Task is not found because it hasn't reached queue yet (unlikely)
      -- \* Task is not found because it hasn't been recorded in execution store yet (more likely)
      ExecNotFound ->
        putStrLn
          $ "Status Response: Task "
          <> show (unTaskId tid)
          <> " not found. "
          <> "The task may not have been started yet.\n"
