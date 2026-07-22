{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Users where

import qualified CommentedExports     (used)
import qualified CppExportNoDirective (alpha)
import           Data.Coerce          (coerce)
import qualified DeprecatedHeader     (usedHere)
import qualified FieldMiddle          (fieldUsed)
import qualified KeepCtors            (D (..), N (..), Wrap (..))
import qualified MakeExplicitUsed     (T (..), usedByOther)
import qualified MultilineTrim        (T (..), used)
import qualified PatternClash         (pattern T)
import qualified QualifiedReexport    (originUsed)
import qualified ReexportMiddle       (originUsed)
import qualified TrimExports          (T (..), used)
import qualified TrimImplicitCtor     (T (MkA), U)
import           TrimKeywords
import qualified TrimPartialCtor      (T (MkA))
import qualified TrimUnusedCtor       (T (MkA), U)

commented :: Int
commented = CommentedExports.used

cpp :: Int
cpp = CppExportNoDirective.alpha

deprecated :: Int
deprecated = DeprecatedHeader.usedHere

field :: Int
field = FieldMiddle.fieldUsed undefined

newtype Via = Via Int
  deriving (Semigroup) via KeepCtors.Wrap

unN :: KeepCtors.N -> Int
unN = coerce

deriving instance Show KeepCtors.D

makeExplicit :: Int
makeExplicit = case MakeExplicitUsed.MkT MakeExplicitUsed.usedByOther of MakeExplicitUsed.MkT n -> n

multiline :: Int
multiline = case MultilineTrim.MkT MultilineTrim.used of MultilineTrim.MkT n -> n

patternClash :: Bool
patternClash = case PatternClash.T of PatternClash.T -> True

qualifiedReexport :: Int
qualifiedReexport = QualifiedReexport.originUsed

reexportMiddle :: Int
reexportMiddle = ReexportMiddle.originUsed

trimExports :: Int
trimExports = case TrimExports.MkT TrimExports.used of TrimExports.MkT n -> n

implicitCtor :: TrimImplicitCtor.T
implicitCtor = TrimImplicitCtor.MkA

implicitCtorU :: TrimImplicitCtor.U -> Int
implicitCtorU _ = 0

keywords :: Int :+: Bool
keywords = MkSum 1 True

keywordsValue :: Int
keywordsValue = usedValue + Used

partialCtor :: TrimPartialCtor.T
partialCtor = TrimPartialCtor.MkA

unusedCtor :: TrimUnusedCtor.T
unusedCtor = TrimUnusedCtor.MkA

unusedCtorU :: TrimUnusedCtor.U -> Int
unusedCtorU _ = 0
