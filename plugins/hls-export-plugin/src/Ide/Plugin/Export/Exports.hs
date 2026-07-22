module Ide.Plugin.Export.Exports
  ( isExplicit
  , isExported
  , hasCppOrReexport
  , addExport
  , addConstructorExport
  , removeExport
  , removeConstructorExport
  , keepUsedExports
  , keepConstructorsOfUsed
  , addExportList
  , isNameReferencedExternally
  ) where

import           Data.Maybe                         (isJust, isNothing)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Text.Utf16.Rope.Mixed         (Rope)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error          (srcSpanToRange)
import           Development.IDE.GHC.ExactPrint.CPP (spanHasCpp)
import           Development.IDE.Types.Shake        (WithHieDb)
import           HieDb
import           Ide.Plugin.Export.ExactPrint
import           Ide.Plugin.Export.Utils
import           Language.Haskell.GHC.ExactPrint    (makeDeltaAst)
import           Language.LSP.Protocol.Types

isExplicit :: ParsedSource -> Bool
isExplicit = isJust . hsmodExports . unLoc

-- | Also matches names appearing only as constructor children of an 'IEThingWith' parent.
isExported :: RdrName -> ParsedSource -> Bool
isExported n ps = case hsmodExports (unLoc ps) of
  Nothing          -> False
  Just (L _ items) -> any (covers . unLoc) items
  where
    nFS = rdrNameFS n
    covers ie = parentNameIs nFS ie || isInIE nFS ie

-- | Extract the export list and pick an edit strategy: splice surgically when
-- the span holds a CPP directive, otherwise reprint the whole transformed list.
withExportList
  :: Maybe Rope
  -> ParsedSource
  -> (LExportList -> Maybe LExportList)          -- ^ reprint transform
  -> (Range -> LExportList -> Maybe [TextEdit])  -- ^ list holds a directive
  -> Maybe [TextEdit]
withExportList msrc ps reprint onCpp = do
  exports <- hsmodExports (unLoc ps)
  full <- srcSpanToRange (getLoc exports)
  if spanHasCpp msrc full
    then onCpp full exports
    else do
      newList <- reprint (makeDeltaAst exports)
      Just [TextEdit full (printExportList newList)]

addExport :: Maybe Rope -> ParsedSource -> LIE GhcPs -> Maybe [TextEdit]
addExport msrc ps item =
  withExportList msrc ps (Just . appendIE item) $ \full _ ->
    Just [insertAfterOpen full (printIE item)]

addConstructorExport :: Maybe Rope -> RdrName -> RdrName -> ParsedSource -> Maybe [TextEdit]
addConstructorExport msrc parent ctor ps =
  withExportList msrc ps (addCtorUnderParent parent ctor) $ \full exports ->
    (\txt -> [insertAfterOpen full txt]) <$> freshCtorEntry parent ctor (unLoc exports)

-- | Splice @itemTxt@ in right after the opening paren with a trailing comma,
-- @( <itemTxt>, <existing> )@.
insertAfterOpen :: Range -> Text -> TextEdit
insertAfterOpen (Range (Position sl sc) _) itemTxt =
  TextEdit (Range pos pos) (" " <> itemTxt <> ",")
  where
    -- `sc` is the column of `(`, so insert just past it.
    pos = Position sl (sc + 1)

-- | Reprinting would drop the directives the parser stripped, so unexport is
-- declined when the export list holds a directive.
removeExport :: Maybe Rope -> ParsedSource -> RdrName -> Maybe [TextEdit]
removeExport msrc ps name =
  withExportList msrc ps (removeMatchingIE matches) declineUnderCpp
  where
    matches = parentNameIs (rdrNameFS name)

removeConstructorExport :: Maybe Rope -> RdrName -> RdrName -> ParsedSource -> Maybe [TextEdit]
removeConstructorExport msrc parent ctor ps =
  withExportList msrc ps (removeCtorUnderParent parent ctor) declineUnderCpp

-- | An 'onCpp' handler that declines: the edit has no safe surgical form, so it
-- is offered only when the list reprints cleanly.
declineUnderCpp :: Range -> LExportList -> Maybe [TextEdit]
declineUnderCpp _ _ = Nothing

hasCppOrReexport :: ModSummary -> Maybe Rope -> ParsedSource -> Bool
hasCppOrReexport summ msrc ps =
  -- Without the file text, we cannot see the CPP directives.
  (isCppModule summ && isNothing msrc)
    || maybe headerHasCpp listHasCppOrReexport (hsmodExports (unLoc ps))
  where
    modl = unLoc ps
    headerHasCpp = maybe False (spanHasCpp msrc) $ do
      Range _ nameEnd <- srcSpanToRange . getLoc =<< hsmodName modl
      let bodyStart = case map getLocA (hsmodImports modl) ++ map getLocA (hsmodDecls modl) of
            sp : _ | Just (Range start _) <- srcSpanToRange sp -> start
            _ -> Position maxBound 0
      Just (Range nameEnd bodyStart)
    listHasCppOrReexport exports =
      any (isModuleContents . unLoc) (unLoc exports)
        || maybe False (spanHasCpp msrc) (srcSpanToRange (getLoc exports))
    isModuleContents IEModuleContents{} = True
    isModuleContents _                  = False

-- | Remove the entries and the children that no other module uses. An entry
-- stays when another module uses any of its names.
keepUsedExports :: Maybe Rope -> ParsedSource -> (Name -> Bool) -> [AvailInfo] -> Maybe [TextEdit]
keepUsedExports msrc ps wanted avails =
  withExportList msrc ps (\l -> Just (foldl' trim l avails)) declineUnderCpp
  where
    trim l avail
      | any wanted (availNames avail) =
          foldl' (removeChild parent) l (filter (not . wanted) (children avail))
      | otherwise = removeAll (removeMatchingIE (isEntryFor parent)) l
      where
        parent = availName avail
    children (AvailTC parent names pieces) = filter (/= parent) names ++ map flSelector pieces
    children _ = []
    isEntryFor n ie = parentNameIs (getOccFS n) ie && isTypeIE ie == isTyConName n
    removeChild parent l c = removeAll (removeCtorUnderParent (toRdr parent) (toRdr c)) l
    -- Disambiguate between type and pattern synonyms.
    isTypeIE IEVar{} = False
    isTypeIE _       = True
    removeAll f l = maybe l (removeAll f) (f l)
    toRdr = mkRdrUnqual . getOccName

-- | Also keep every constructor of a used type, importers can need a
-- constructor referencing it, e.g. for standalone deriving or coerce.
keepConstructorsOfUsed :: (Name -> Bool) -> [AvailInfo] -> Name -> Bool
keepConstructorsOfUsed used avails = \n -> used n || n `elemNameSet` constructors
  where
    constructors = mkNameSet
      [ c | avail <- avails, any used (availNames avail), c <- availNames avail, isDataConName c ]

addExportList :: ParsedSource -> [LIE GhcPs] -> Maybe [TextEdit]
addExportList ps items = do
  Range _ end <- anchor
  Just [TextEdit (Range end end) (" (" <> T.intercalate ", " (map printIE items) <> ")")]
  where
    modl = unLoc ps
    -- A module warning pragma sits between the module name and the export list.
    anchor = case hsmodDeprecMessage (hsmodExt modl) of
      Just lwarn -> srcSpanToRange (getLoc lwarn)
      Nothing    -> srcSpanToRange . getLoc =<< hsmodName modl

isNameReferencedExternally :: WithHieDb -> [FilePath] -> Name -> IO Bool
isNameReferencedExternally withDb exclude n = case nameModule_maybe n of
  Nothing  -> pure False
  Just mod -> do
    rows <- withDb $ \db ->
      findReferences db True (nameOccName n) (Just (moduleName mod)) (Just (moduleUnit mod)) exclude
    pure (any (external mod) rows)
  where
    -- Skip the uses that GHC inserts. See Note [Generated references] in the
    -- rename plugin.
    external mod row@(refRow :. _) =
      not (refIsGenerated refRow)
        && not (definedBy mod row)
    definedBy mod (_ :. info) =
      modInfoName info == moduleName mod && modInfoUnit info == moduleUnit mod
