{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import Data.Map.Strict qualified as Map
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Relude
import UnliftIO.Async (Async)

----------------------------------------------------------------------------------------------------
-- Driver Messages
----------------------------------------------------------------------------------------------------

newtype DriverMessage = DriverMessage
  {unDriverMessage :: Text}
  deriving (Eq, Show)

----------------------------------------------------------------------------------------------------
-- Work Scheduling
----------------------------------------------------------------------------------------------------

-- | Request wrapper signalling whether worker threads should continue or shutdown.
-- Isomorphic to `Maybe Task`.
--
--  Used for (async) exception-less control flow.
data DoOrDie
  = Shutdown
  | Run RequestType
  deriving (Eq, Show)

----------------------------------------------------------------------------------------------------
-- Requests
----------------------------------------------------------------------------------------------------

-- | Request flavors. Shutdowns are covered by `DoOrDie`
--
-- TODO 1 - Give `RequestInvalid` a text parameter for sharing information about failure mode
-- TODO 2 - Add new request to populate task library with user-defined tasks or handle elsewhere
data RequestType
  = RequestHelp
  | RequestInvalid
  | RequestValid Task
  | RequestCancel TaskId
  | RequestStatusCheck TaskId
  deriving (Eq, Show)

----------------------------------------------------------------------------------------------------
-- Tasks
----------------------------------------------------------------------------------------------------

-- Task DSL

-- | A "Task" is a ordered sequence of steps.
data Task = Task
  { -- | Unique identifier assigned by the executor, rather than user-provided. Used for tracking, reporting, etc
    taskId :: TaskId,
    -- | Configurable options used during task's execution. User-provided.
    taskConfig :: TaskConfig,
    -- | The sequence of steps to be executed when this task is run. User-provided.
    taskSteps :: [StepType]
  }
  deriving (Eq, Show)

-- | A task identifier using common UUIDv4 format. See https://www.uuidtools.com/uuid-versions-explained.
--
-- NOTE: UUIDv7 is better, but isn't available. https://github.com/haskell-hvr/uuid/issues/76
-- For more info: https://uuid7.com/, https://buildkite.com/blog/goodbye-integers-hello-uuids
newtype TaskId = TaskId {unTaskId :: UUID} deriving (Eq, Show, Read, Ord)

mkTaskId :: IO TaskId
mkTaskId = TaskId <$> nextRandom

-- | Placeholder configuration record for `Task`s
data TaskConfig
  = DefaultTaskConfig
  deriving (Eq, Show)

-- | Action primitives used when defining tasks.
data StepType
  = NoOp
  | SleepShort
  | SleepMid
  | SleepLong
  deriving (Eq, Show)

newtype TaskName = TaskName
  {unTaskName :: Text}
  deriving (Eq, Ord, Show)

-- TODO Allow dynamic updates so users can register user-defined tasks. Not bothering now since
-- using an actual database would be better for this
taskLibrary :: Map TaskName [StepType]
taskLibrary =
  exampleTasks
    <> Map.fromList
      [ (TaskName "noop", [NoOp]),
        (TaskName "short", [SleepShort]),
        (TaskName "mid", [SleepMid]),
        (TaskName "long", [NoOp])
      ]

----------------------------------------------------------------------------------------------------
-- Example User-Defined Tasks

-- | Example tasks
--  NOTE: Rather than adding a sum of "task types" (Bubble | Squirrel ...) that lives inside the
-- application internals, define our examples as plain values.
--
--  This makes them easy to externalize/push upwards and have users define tasks using our
--  "Task" library/DSL
bubbleBathOptimization, squirrelPatrol, unicornWrangling :: [StepType]
bubbleBathOptimization = [NoOp]
squirrelPatrol = [SleepShort]
unicornWrangling = [NoOp, SleepLong, NoOp, SleepMid, NoOp, SleepLong]

-- | Hardcoded commands for parser and the example tasks they map to
exampleTasks :: Map.Map TaskName [StepType]
exampleTasks =
  Map.fromList
    $ join
      [ nameTask bubbleBathOptimization <$> ["bbo", "bubble", "bubbleBathOptimization"],
        nameTask squirrelPatrol <$> ["sp", "squirrel", "squirrelPatrol"],
        nameTask unicornWrangling <$> ["uw", "unicorn", "unicornWrangling"]
      ]
  where
    nameTask task name = (TaskName name, task)

----------------------------------------------------------------------------------------------------
-- Executions
----------------------------------------------------------------------------------------------------

-- Execution Checkpointing
data ExecStatus
  = ExecNotFound
  | ExecInProgress
  | ExecCompleted
  | ExecCancelled
  deriving (Eq, Show)

-- Execution Tracking

newtype ExecutionStore = ExecutionStore
  {unExecutionStore :: Map TaskId AsyncExecution}

-- | A job scheduled for asynchronous execution that is tracked in the `ExecutionStore`.
-- Create an `Execution` with `scheduleExecution`.
newtype AsyncExecution = AsyncExecution
  {unAsyncExecution :: Async ()}
