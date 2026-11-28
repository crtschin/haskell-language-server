{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Users where

import           Data.Coerce  (coerce)
import qualified TrimExports  (D (..), N (..), Q (QA), T (MkA), Wrap (..),
                               fieldUsed, originUsed, pattern Clash, qualUsed,
                               used)
import qualified TrimImplicit (T (MkA), U)
import           TrimImplicit (pattern Used, type (:+:) (..), usedValue)

trimExports :: Int
trimExports = TrimExports.used + TrimExports.originUsed + TrimExports.qualUsed

field :: Int
field = TrimExports.fieldUsed undefined

partialCtor :: TrimExports.Q
partialCtor = TrimExports.QA

unusedCtor :: TrimExports.T
unusedCtor = TrimExports.MkA

patternClash :: Bool
patternClash = case TrimExports.Clash of TrimExports.Clash -> True

newtype Via = Via Int
  deriving (Semigroup) via TrimExports.Wrap

unN :: TrimExports.N -> Int
unN = coerce

deriving instance Show TrimExports.D

implicitCtor :: TrimImplicit.T
implicitCtor = TrimImplicit.MkA

implicitCtorU :: TrimImplicit.U -> Int
implicitCtorU _ = 0

keywords :: Int :+: Bool
keywords = MkSum 1 True

keywordsValue :: Int
keywordsValue = usedValue + Used
