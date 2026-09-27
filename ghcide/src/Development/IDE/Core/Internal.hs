-- | The core of the query API, for ghcide and the packages that build on it.
module Development.IDE.Core.Internal
  ( -- * Reads
    module Development.IDE.Core.Internal.Reads
  , Key (..)
  , Project (..)
  , IsScope (..)
    -- * Reads without a dependency
  , lastValueIO
  , useWithoutDependency
    -- * Queue any action
  , DelayedAction
  , mkDelayedAction
  , delayedAction
  , shakeEnqueue
  , Pending
  , enqueue
  , wait
    -- * Rule definition with an explicit policy
  , defineRule
    -- * Shake core
  , BadDependency (..)
  , IdeRule
  , Q (..)
  , Query (..)
  , QueryFailed (..)
  , ShakeExtras (..)
  , getShakeExtras
  , hasRun
  , Log (..)
  , IdeResult
  , IdeState (..)
  , mRunLspT
    -- * Tracked values
  , Tracked
  , untrackStale
  , tracked
  , untracked
  ) where

import           Development.IDE.Core.Internal.Reads
import           Development.IDE.Core.Internal.Shake
import           Development.IDE.Core.Internal.Tracked
import           Development.IDE.Core.Internal.Types
