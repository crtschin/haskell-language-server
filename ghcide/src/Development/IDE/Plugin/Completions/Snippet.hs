{-# LANGUAGE OverloadedStrings #-}

-- | Top-level snippet completions (module header, imports, declarations) offered
-- at a 'TopContext' position. The valid groups are decided by the context
-- detector, so this module only turns groups into 'CompletionItem's.
module Development.IDE.Plugin.Completions.Snippet (getContextSnippets) where

import           Control.Lens                               ((&), (?~))
import           Data.Text                                  (Text)
import           Development.IDE.Plugin.Completions.Context (ContextGroup (..))
import           Development.IDE.Plugin.Completions.Types   (defaultCompletionItemWithLabel)
import qualified Language.LSP.Protocol.Lens                 as L
import           Language.LSP.Protocol.Types

-- | The snippets valid for the given groups, in group order.
getContextSnippets :: [ContextGroup] -> [CompletionItem]
getContextSnippets = concatMap (map mkSnippet . groupSnippets)

-- | The (label, snippet text) pairs for a group. The text uses LSP tab stops,
-- which is all these snippets need.
groupSnippets :: ContextGroup -> [(Text, Text)]
groupSnippets g = case g of
  HeaderGroup      -> [ ("module", "module ${1:name} where") ]
  ImportGroup      -> [ ("import", "import ${1:module}")
                      , ("import (list)", "import ${1:module} (${2:names})")
                      , ("import qualified", "import qualified ${1:module} as ${2:alias}")
                      , ("import hiding", "import ${1:module} hiding (${2:names})")
                      ]
  DeclarationGroup -> [ ("function", "${1:identifier} :: ${2:type}\n${1:identifier} = ${3:body}")
                      , ("instance", "instance ${1:name} where")
                      , ("class", "class ${1:name} where")
                      ]

mkSnippet :: (Text, Text) -> CompletionItem
mkSnippet (label, contents) =
  defaultCompletionItemWithLabel label
    & L.kind ?~ CompletionItemKind_Snippet
    & L.insertText ?~ contents
    & L.insertTextFormat ?~ InsertTextFormat_Snippet
