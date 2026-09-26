module Development.IDE.Core.Internal
  ( -- * Reads without a dependency
    lastValueIO
  , useWithoutDependency
    -- * Queue any action
  , DelayedAction
  , mkDelayedAction
  , delayedAction
  , shakeEnqueue
    -- * Rule definition with an explicit policy
  , defineRule
  , DiagnosticSink (..)
  ) where

import           Development.IDE.Core.Internal.Shake
