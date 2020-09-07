module Graphics.SvgTree.Printer
  ( ppTree,
    ppDocument,
  )
where

import Control.Lens ((^.))
import Data.List
import Graphics.SvgTree.Types hiding (Element)
import Graphics.SvgTree.XmlParser
import Text.XML.Light hiding (showAttr)

ppDocument :: Document -> String
ppDocument doc =
  ppElementS_ (_documentElements doc) (xmlOfDocument doc) ""

ppTree :: Tree -> String
ppTree t = ppTreeS t ""

ppTreeS :: Tree -> ShowS
ppTreeS tree =
  case xmlOfTree tree of
    Just x -> ppElementS_ (treeChildren tree) x
    Nothing -> id

treeChildren :: Tree -> [Tree]
treeChildren t = case t of
  GroupTree g -> g ^. groupChildren
  SymbolTree g -> g ^. groupChildren
  DefinitionTree g -> g ^. groupChildren
  ClipPathTree c -> c ^. clipPathContent
  PatternTree p -> p ^. patternElements
  MarkerTree m -> m ^. markerElements
  MaskTree m -> m ^. maskContent
  _ -> []

ppElementS_ :: [Tree] -> Element -> ShowS
ppElementS_ [] e xs | not (null (elContent e)) = ppElement e ++ xs
ppElementS_ children e xs = tagStart name (elAttribs e) $
  case children of
    []
      | "?" `isPrefixOf` qName name -> showString " ?>" xs
      | otherwise -> showString " />" xs
    _ -> showChar '>' (foldr ppTreeS (tagEnd name xs) children)
  where
    name = elName e

--------------------------------------------------------------------------------
tagStart :: QName -> [Attr] -> ShowS
tagStart qn as rs = '<' : showQName qn ++ as_str ++ rs
  where
    as_str = if null as then "" else ' ' : unwords (map showAttr as)

showAttr :: Attr -> String
showAttr (Attr qn v) = showQName qn ++ '=' : '"' : v ++ "\""
