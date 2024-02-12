{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App where

import Control.Concurrent.STM.TBMQueue (closeTBMQueue)
import Control.Lens (view)
import GHC.IO.Handle (hClose)
import Options.Generic
  ( getRecord,
    unHelpful,
  )
import Processor.Execution (newExecutionStore)
import Processor.Task
  ( newTaskQueue,
  )
import Relude
import Scheduler (doTasks, requestTasks)
import Settings
  ( ExecCfg (ExecCfg),
    ExecOpts (limit),
    HasExecCfg (execCfgTaskQueue),
  )
import UnliftIO.Async (concurrently_)
import UnliftIO.Exception (finally)

initSettings :: IO ExecCfg
initSettings = do
  opts :: ExecOpts <- getRecord "Executor Program"

  let maxConcurrentTasks = unHelpful $ limit opts

  taskQueue <- newTaskQueue maxConcurrentTasks -- FIXME Stop using TBMQueue and handle max concurrent with unbounded queue
  execStore <- newExecutionStore

  pure $ ExecCfg taskQueue execStore maxConcurrentTasks

runApp' :: ExecCfg -> IO ()
runApp' cfg =
  flip runReaderT cfg
    $ concurrently_ requestTasks doTasks
    `finally` do
      liftIO do
        hClose stdin
        hClose stdout
      atomically . closeTBMQueue $ view execCfgTaskQueue cfg

runApp :: IO ()
runApp = do
  initSettings >>= runApp'