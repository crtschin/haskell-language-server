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
    -- * Shake core
  , BadDependency (..)
  , IdeRule
  , IdeResult
  , Q (..)
  , ShakeExtras (..)
  , getShakeExtras
  , getValues
  , askShake
  , IdeAction (..)
  , Log (..)
  , mRunLspT
  ) where

import           Development.IDE.Core.Internal.Shake
