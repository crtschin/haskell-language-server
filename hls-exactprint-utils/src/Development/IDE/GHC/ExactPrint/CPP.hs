{-# LANGUAGE OverloadedStrings #-}

module Development.IDE.GHC.ExactPrint.CPP
  ( spanHasCpp
  , isCppDirective
  , deleteOneClean
  ) where

import           Data.Maybe                  (catMaybes, listToMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Utf16.Rope.Mixed  (Rope)
import           Development.IDE.Core.Text   (takeLineRange)
import           Development.IDE.GHC.Error   (srcSpanToRange)
import           GHC                         (SrcSpan)
import           Language.LSP.Protocol.Types (Position (..), Range (..),
                                              TextEdit (..))

-- | Whether the source over @range@ holds a CPP directive.
spanHasCpp :: Maybe Rope -> Range -> Bool
spanHasCpp Nothing _ = False
spanHasCpp (Just rope) (Range (Position l0 _) (Position l1 _)) =
  any isCppDirective (takeLineRange (fromIntegral l0) (fromIntegral l1) rope)

-- | Whether a line is a CPP directive. In a source compiled with CPP a directive
-- is the only line whose first non-space character is @#@.
isCppDirective :: Text -> Bool
isCppDirective = T.isPrefixOf "#" . T.stripStart

-- | Surgically delete the one element matching @match@ from a located list,
-- consuming the separator on whichever neighbouring side is free of a CPP
-- directive (preferring the following side, which leaves a preceding element's
-- trailing comment untouched). 'Nothing' when nothing matches or no
-- directive-free side exists (sole element, or both usable gaps cross a
-- directive). Always removes exactly one element and one separator, so the
-- result stays well formed in every CPP configuration.
deleteOneClean :: Maybe Rope -> (a -> SrcSpan) -> (a -> Bool) -> [a] -> Maybe TextEdit
deleteOneClean msrc spanOf match items = case break match items of
  (_, [])            -> Nothing
  (pre, target:post) -> do
    Range tStart tEnd <- srcSpanToRange (spanOf target)
    let following = do
          next <- listToMaybe post
          Range nStart _ <- srcSpanToRange (spanOf next)
          Just (Range tStart nStart)
        preceding = do
          prev <- listToMaybe (reverse pre)
          Range _ pEnd <- srcSpanToRange (spanOf prev)
          Just (Range pEnd tEnd)
    range <- listToMaybe (filter (not . spanHasCpp msrc) (catMaybes [following, preceding]))
    Just (TextEdit range mempty)
