{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Settings where

import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Control.Lens (makeClassy)
import Options.Generic
  ( ParseRecord,
    type (<?>),
  )
import Relude
import Types (ExecutionStore, Task)

----------------------------------------------------------------------------------------------------
-- CLI Options

newtype ExecOpts = ExecOpts
  { -- | Configurable limit of tasks an executor can run concurrently.
    limit :: Int <?> "Max number of concurrent tasks per executor"
  }
  deriving (Show, Eq, Generic)

instance ParseRecord ExecOpts

----------------------------------------------------------------------------------------------------
-- Configuration

data ExecCfg = ExecCfg
  { -- | Queue of tasks to be scheduled and executed
    _execCfgTaskQueue :: TBMQueue Task,
    -- | Map of async actions used to fulfill requests and tasks
    _execCfgExecutionStore :: TMVar ExecutionStore,
    -- | Max number of concurrent tasks per executor
    _execCfgConcurrentTaskLimit :: Int
  }

makeClassy ''ExecCfg
