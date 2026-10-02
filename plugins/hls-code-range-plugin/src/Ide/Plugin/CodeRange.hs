{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Ide.Plugin.CodeRange (
    descriptor
    , Log

    -- * Internal
    , findPosition
    , findFoldingRanges
    , createFoldingRange
    ) where

import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Trans.Except    (ExceptT)
import           Data.List.Extra               (drop1)
import           Data.Maybe                    (fromMaybe)
import           Data.Vector                   (Vector)
import qualified Data.Vector                   as V
import           Development.IDE               (IdeState, Range (Range),
                                                Recorder, WithPriority,
                                                cmapWithPrio)
import           Development.IDE.Core.API      (Tracked (..), fastForward,
                                                fetch_, rewind, runQuery,
                                                settle_)
import           Ide.Logger                    (Pretty (..))
import           Ide.Plugin.CodeRange.Rules    (CodeRange (..),
                                                GetCodeRange (..),
                                                codeRangeRule, crkToFrk)
import qualified Ide.Plugin.CodeRange.Rules    as Rules (Log)
import           Ide.Plugin.Error
import           Ide.PluginUtils               (positionInRange)
import           Ide.Types                     (PluginDescriptor (pluginHandlers, pluginRules),
                                                PluginId, PluginMethodHandler,
                                                defaultPluginDescriptor,
                                                mkPluginHandler)
import           Language.LSP.Protocol.Message (Method (Method_TextDocumentFoldingRange, Method_TextDocumentSelectionRange),
                                                SMethod (SMethod_TextDocumentFoldingRange, SMethod_TextDocumentSelectionRange))
import           Language.LSP.Protocol.Types   (FoldingRange (..),
                                                FoldingRangeParams (..),
                                                NormalizedFilePath, Null,
                                                Position (..), Range (_start),
                                                SelectionRange (..),
                                                SelectionRangeParams (..),
                                                TextDocumentIdentifier (TextDocumentIdentifier),
                                                Uri, type (|?) (InL))
import           Prelude                       hiding (log, span)

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId "Provides selection and folding ranges for Haskell")
    { pluginHandlers = mkPluginHandler SMethod_TextDocumentSelectionRange (selectionRangeHandler recorder)
    <> mkPluginHandler SMethod_TextDocumentFoldingRange (foldingRangeHandler recorder)
    , pluginRules = codeRangeRule (cmapWithPrio LogRules recorder)
    }

newtype Log = LogRules Rules.Log

instance Pretty Log where
    pretty (LogRules codeRangeLog) = pretty codeRangeLog


foldingRangeHandler :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'Method_TextDocumentFoldingRange
foldingRangeHandler _ ide _ FoldingRangeParams{..} =
    do
        filePath <- getNormalizedFilePathE uri
        foldingRanges <- findFoldingRanges <$> runQuery ide (fetch_ GetCodeRange filePath)
        pure . InL $ foldingRanges
  where
    uri :: Uri
    TextDocumentIdentifier uri = _textDocument

selectionRangeHandler :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'Method_TextDocumentSelectionRange
selectionRangeHandler _ ide _ SelectionRangeParams{..} = do
   do
        filePath <- getNormalizedFilePathE uri
        getSelectionRanges ide filePath positions
  where
    uri :: Uri
    TextDocumentIdentifier uri = _textDocument

    positions :: [Position]
    positions = _positions


getSelectionRanges :: MonadIO m => IdeState -> NormalizedFilePath -> [Position] -> ExceptT PluginError m ([SelectionRange] |? Null)
getSelectionRanges ide file positions = runQuery ide $ do
    Tracked codeRange mapping <- settle_ GetCodeRange file
    positions' <- traverse (rewind mapping) positions
    let selectionRanges = flip fmap positions' $ \pos ->
            -- We need a default selection range if the lookup fails,
            -- so that other positions can still have valid results.
            let defaultSelectionRange p = SelectionRange (Range p p) Nothing
             in (\p -> fromMaybe (defaultSelectionRange p) . findPosition p) <$> pos <*> codeRange
    InL <$> fastForward mapping (sequenceA selectionRanges)

-- | Find 'Position' in 'CodeRange'. This can fail, if the given position is not covered by the 'CodeRange'.
findPosition :: Position -> CodeRange -> Maybe SelectionRange
findPosition pos root = go Nothing root
  where
    -- Helper function for recursion. The range list is built top-down
    go :: Maybe SelectionRange -> CodeRange -> Maybe SelectionRange
    go acc node =
        if positionInRange pos range
        then maybe acc' (go acc') (binarySearchPos children)
        -- If all children doesn't contain pos, acc' will be returned.
        -- acc' will be Nothing only if we are in the root level.
        else Nothing
      where
        range = _codeRange_range node
        children = _codeRange_children node
        acc' = Just $ maybe (SelectionRange range Nothing) (SelectionRange range . Just) acc

    binarySearchPos :: Vector CodeRange -> Maybe CodeRange
    binarySearchPos v
        | V.null v = Nothing
        | V.length v == 1,
            Just r <- V.headM v = if positionInRange pos (_codeRange_range r) then Just r else Nothing
        | otherwise = do
            let (left, right) = V.splitAt (V.length v `div` 2) v
            startOfRight <- _start . _codeRange_range <$> V.headM right
            if pos < startOfRight then binarySearchPos left else binarySearchPos right

-- | Traverses through the code range and it children to a folding ranges.
--
-- It starts with the root node, converts that into a folding range then moves towards the children.
-- It converts each child of each root node and parses it to folding range and moves to its children.
--
-- Two cases to that are assumed to be taken care on the client side are:
--
--      1. When a folding range starts and ends on the same line, it is upto the client if it wants to
--      fold a single line folding or not.
--
--      2. As we are converting nodes of the ast into folding ranges, there are multiple nodes starting from a single line.
--      A single line of code doesn't mean a single node in AST, so this function removes all the nodes that have a duplicate
--      start line, ie. they start from the same line.
--      Eg. A multi-line function that also has a multi-line if statement starting from the same line should have the folding
--      according to the function.
--
-- We think the client can handle this, if not we could change to remove these in future
--
-- Discussion reference: https://github.com/haskell/haskell-language-server/pull/3058#discussion_r973737211
findFoldingRanges :: CodeRange -> [FoldingRange]
findFoldingRanges codeRange =
    -- removing the first node because it folds the entire file
    drop1 $ findFoldingRangesRec codeRange

findFoldingRangesRec :: CodeRange -> [FoldingRange]
findFoldingRangesRec r@(CodeRange _ children _) =
    let frChildren :: [FoldingRange] = concat $ V.toList $ fmap findFoldingRangesRec children
    in case createFoldingRange r of
        Just x  -> x:frChildren
        Nothing -> frChildren

-- | Parses code range to folding range
createFoldingRange :: CodeRange -> Maybe FoldingRange
createFoldingRange (CodeRange (Range (Position lineStart charStart) (Position lineEnd charEnd)) _ ck) = do
    -- Type conversion of codeRangeKind to FoldingRangeKind
    let frk = crkToFrk ck
    Just (FoldingRange lineStart (Just charStart) lineEnd (Just charEnd) (Just frk) Nothing)
