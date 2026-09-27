{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeData               #-}
-- | Whether the rule of a key publishes diagnostics.
module Development.IDE.Core.Internal.Publishing
  ( type Publishing (..)
  , KnownPublishing (..)
  , RuleDiagnostics
  ) where

import           Data.Proxy (Proxy)

-- | Whether a rule publishes diagnostics.
type data Publishing = Publishes | Quiet

class KnownPublishing (p :: Publishing) where
  publishes :: Proxy p -> Bool

instance KnownPublishing Publishes where
  publishes _ = True

instance KnownPublishing Quiet where
  publishes _ = False

-- | Declares whether the rule of the key @k@ publishes diagnostics:
--
-- @
-- data GetModIface = GetModIface
--   deriving anyclass (RuleDiagnostics Quiet)
-- @
class KnownPublishing p => RuleDiagnostics (p :: Publishing) k | k -> p
