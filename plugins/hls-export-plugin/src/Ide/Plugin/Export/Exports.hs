{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards  #-}

module Ide.Plugin.Export.Exports
  ( isExplicit
  , isExported
  , addExport
  , addConstructorExport
  , removeExport
  , removeConstructorExport
  , rdrNameText
  )
where

import           Data.List                       (find)
import           Data.Maybe                      (isJust, listToMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util (FastString)
import           Development.IDE.GHC.Error       (srcSpanToRange)
import           Development.IDE.GHC.Util        (printRdrName)
import           Language.LSP.Protocol.Types

isExplicit :: ParsedSource -> Bool
isExplicit = isJust . hsmodExports . unLoc

-- | Also matches names appearing only as constructor children of an 'IEThingWith' parent.
isExported :: RdrName -> ParsedSource -> Bool
isExported n ps = case hsmodExports (unLoc ps) of
  Nothing      -> False
  Just (L _ items) -> any (covers . unLoc) items
  where
    nFS = rdrNameFS n
    covers ie =
      maybe False ((== nFS) . rdrNameFS) (ieParentName ie)
        || case ie of
             IEThingWith _ _ _ cs _ ->
               any ((== nFS) . rdrNameFS . ieWrappedRdrName . unLoc) cs
             _ -> False

rdrNameFS :: RdrName -> FastString
rdrNameFS = occNameFS . rdrNameOcc

data InsertPoint = InsertPoint
  { position :: !Position,
    isEmpty  :: !Bool
  }

stepLeft :: Position -> Position
stepLeft (Position l c) = Position l (max 0 (c - 1))

exportListInsertPoint :: ParsedSource -> Maybe InsertPoint
exportListInsertPoint ps = do
  L lloc items <- hsmodExports (unLoc ps)
  case items of
    [] -> do
      Range _ end <- srcSpanToRange (locA lloc)
      Just (InsertPoint (stepLeft end) True)
    (_ : _) -> do
      Range _ end <- srcSpanToRange (locA (getLoc (last items)))
      Just (InsertPoint end False)

parentEntryInfo :: RdrName -> ParsedSource -> Maybe (Range, IE GhcPs)
parentEntryInfo parent ps = do
  items <- currentExports ps
  findEntry items
  where
    parentFS = rdrNameFS parent
    findEntry [] = Nothing
    findEntry (L l ie : rest)
      | maybe False ((== parentFS) . rdrNameFS) (ieParentName ie),
        Just r <- srcSpanToRange (locA l) =
          Just (r, ie)
      | otherwise = findEntry rest

-- | Appends @name@ verbatim after the last entry; callers pre-format suffixes such as @\"T (..)\"@.
addExport :: ParsedSource -> Text -> Maybe [TextEdit]
addExport ps name = do
  InsertPoint{..} <- exportListInsertPoint ps
  let text = if isEmpty then name else ", " <> name
  Just [TextEdit (Range position position) text]

-- | Merges @ctor@ into an existing parent entry; 'Nothing' when already covered by @T(..)@ or already listed.
addConstructorExport :: RdrName -> RdrName -> ParsedSource -> Maybe [TextEdit]
addConstructorExport parent ctor ps = case parentEntryInfo parent ps of
  Nothing -> addExport ps (rdrNameText parent <> " (" <> rdrNameText ctor <> ")")
  Just (r, entry) -> case entry of
    IEThingAll {} -> Nothing
    IEThingWith _ _ _ cs _
      | any ((== ctorFS) . rdrNameFS . ieWrappedRdrName . unLoc) cs -> Nothing
      | otherwise ->
          let Range _ end = r
              beforeParen = stepLeft end
           in Just [TextEdit (Range beforeParen beforeParen) (", " <> rdrNameText ctor)]
    IEThingAbs {} ->
      Just [TextEdit r (rdrNameText parent <> " (" <> rdrNameText ctor <> ")")]
    _ -> Nothing
  where
    ctorFS = rdrNameFS ctor

-- | Also strips the adjacent comma so the remaining list stays well-formed.
removeExport :: ParsedSource -> RdrName -> Maybe [TextEdit]
removeExport ps name = do
  items <- currentExports ps
  let indexed = zip [0 :: Int ..] items
  (idx, L loc _) <- find (\(_, L _ ie) -> maybe False ((== nameFS) . rdrNameFS) (ieParentName ie)) indexed
  r <- srcSpanToRange (locA loc)
  deleteRange <- computeDeleteRange r idx items
  Just [TextEdit deleteRange ""]
  where
    nameFS = rdrNameFS name
    computeDeleteRange r idx items
      | idx == 0, length items == 1 = Just r
      | idx == 0 = do
          L nextLoc _ <- listToMaybe (drop 1 items)
          Range nextStart _ <- srcSpanToRange (locA nextLoc)
          let Range start _ = r
          Just (Range start nextStart)
      | otherwise = do
          L prevLoc _ <- listToMaybe (drop (idx - 1) items)
          Range _ prevEnd <- srcSpanToRange (locA prevLoc)
          let Range _ end = r
          Just (Range prevEnd end)

removeConstructorExport :: RdrName -> RdrName -> ParsedSource -> Maybe [TextEdit]
removeConstructorExport parent ctor ps = do
  items <- currentExports ps
  L ploc ie <- find (\(L _ ie) -> maybe False ((== parentFS) . rdrNameFS) (ieParentName ie)) items
  parentRange <- srcSpanToRange (locA ploc)
  case ie of
    IEThingWith _ _ _ cs _ -> do
      let indexed = zip [0 :: Int ..] cs
      (idx, L cloc _) <- find (\(_, L _ iwn) -> rdrNameFS (ieWrappedRdrName iwn) == ctorFS) indexed
      childRange <- srcSpanToRange (locA cloc)
      if null (drop 1 cs)
        -- Replace T(C) with bare T rather than leaving T().
        then Just [TextEdit parentRange (rdrNameText parent)]
        else do
          deleteRange <- computeChildDeleteRange childRange idx cs
          Just [TextEdit deleteRange ""]
    -- T(..) and bare T can't have a child removed
    _ -> Nothing
  where
    parentFS = rdrNameFS parent
    ctorFS = rdrNameFS ctor
    computeChildDeleteRange childRange idx cs
      -- Consume forward ("C, ") to avoid a leading comma.
      | idx == 0 = do
          L nextLoc _ <- listToMaybe (drop 1 cs)
          Range nextStart _ <- srcSpanToRange (locA nextLoc)
          let Range start _ = childRange
          Just (Range start nextStart)
      -- Consume backward (", C") to avoid a trailing comma.
      | otherwise = do
          L prevLoc _ <- listToMaybe (drop (idx - 1) cs)
          Range _ prevEnd <- srcSpanToRange (locA prevLoc)
          let Range _ end = childRange
          Just (Range prevEnd end)

rdrNameText :: RdrName -> Text
rdrNameText = T.pack . printRdrName

ieParentName :: IE GhcPs -> Maybe RdrName
ieParentName = \case
  IEVar _ (L _ wn) _           -> Just (ieWrappedRdrName wn)
  IEThingAbs _ (L _ wn) _      -> Just (ieWrappedRdrName wn)
  IEThingAll _ (L _ wn) _      -> Just (ieWrappedRdrName wn)
  IEThingWith _ (L _ wn) _ _ _ -> Just (ieWrappedRdrName wn)
  _                            -> Nothing

ieWrappedRdrName :: IEWrappedName GhcPs -> RdrName
ieWrappedRdrName = \case
  IEName _ (L _ rdr)    -> rdr
  IEPattern _ (L _ rdr) -> rdr
  IEType _ (L _ rdr)    -> rdr
