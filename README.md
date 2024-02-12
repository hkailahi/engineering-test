# Async Job Queue Application

## Overview

This repo contains a basic async job queue.

Using the Task API, users can define "tasks" that they want to be executed. Via the application driver, users send requests to do some work like:
* Having their user-defined tasks or default tasks executed by the executor
* Cancelling a currently queued/running task
* Polling to see the current state of a currently queued/running task (in-progress, completed, cancelled)

The driver interfaces with an executor by which sends messages over STDIN. The executor schedules and tracks work, and issues responses over STDOUT. The executor works by kicking off two concurrent threads - a request-handler thread and a execution-handler thread.

Driver messages are interpreted into "requests". Requests that can be translated into tasks are pushed onto a task queue and picked up execution by a seperate executor thread. All others (shutdown, cancel, execution status check, invalid task, etc) are fulfilled directly by request handling thread as they don't involve the task queue.

The execution-handling thread maintains an "execution store" that tracks all task executions (past and present) with an unique task identifier. Users can use these identifiers to issues status check and cancellation requests.

### Architecture

Below is a pseudocode representation of this application and its async job queue.

It forks one worker thread to gets user requests, parses reqs into tasks, put tasks on the queue,
  and forks a second worker thread to pick up tasks off the queue for execution.

```hs
type ExecutionStore = Map TaskId (Async ())
type TaskQueue = TMQueue Task -- https://hackage.haskell.org/package/stm-chans-3.0.0.9/docs/Control-Concurrent-STM-TMQueue.html

-- | Responds to user requests from driver.
--
-- Recieves requests, executes unqueueable request types, and populates task queue.
requestTasks' :: TaskQueue -> IO ()
requestTasks' queue =
  fix $ \loop -> do
    req <- getRequest -- Close task queue if issued "shutdown" request
    task <- parseTask req
    enqueueTask task queue
    loop

-- | Executes tasks.
--
--  Consumes task queue, invokes executions, and tracks executions by populating the execution store
doTasks' :: TaskQueue -> ExecutionStore -> IO ()
doTasks' queue store =
  fix $ \loop -> do
    unlessM (atomically $ isClosedTBMQueue queue) do
      atomically (readTBMQueue queue) >>= \case
        Nothing -> do
          loop
        Just task -> do
          liftIO $ withAsync (execTask task) \runner -> do
            executions <- atomically $ readTMVar store
            atomically . writeTMVar store $! Map.insert (taskId task) runner executions
            waitCatch runner >>= \case
               Left _ -> reportTaskCancelled
               Right _ -> reportTaskCompleted
            loop

main :: IO ()
main = do
  queue <- newTBMQueueIO
  store <- newTMVarIO mempty
  concurrently_ (requestTasks' queue) (doTasks' queue store)
```


## Docs

See [docs](./docs) folder for
* [`assignement.md`](./docs/assignment.md)
  * Assignment (original scarf-sh/README.md)
* [`notes.md`](./docs/assignment.md)
  * notes, brainstorms, assumptions, tradeoffs, bugs, etc
* [`instructions.md`](./docs/instructions.md)
  * How-To-Run `engineering-test` guide and initial project setup I used
