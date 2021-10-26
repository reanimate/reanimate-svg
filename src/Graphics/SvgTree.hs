-- | Module providing basic input/output for the SVG document,
-- for document building, please refer to "Graphics.SvgTree.Types".
module Graphics.SvgTree
  ( -- * Saving/Loading functions
    loadSvgFile,
    parseSvgFile,
    parseSvg,
    unparse,
    xmlOfDocument,
    xmlOfTree,
    saveXmlFile,

    -- * Manipulation functions
    cssApply,
    cssRulesOfText,
    -- , applyCSSRules
    -- , resolveUses

    -- * Type definitions
    module Graphics.SvgTree.Types,
  )
where

import Control.Lens
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Graphics.SvgTree.CssParser (cssRulesOfText)
import Graphics.SvgTree.CssTypes
import Graphics.SvgTree.Types
import Graphics.SvgTree.XmlParser
import Text.XML.Light.Input (parseXMLDoc)
import Text.XML.Light.Output (ppcTopElement, prettyConfigPP)

-- | Try to load an SVG file on disc and parse it as
-- an SVG 'Document'.
loadSvgFile :: FilePath -> IO (Maybe Document)
loadSvgFile filename =
  parseSvgFile filename <$> T.readFile filename

-- | Parse an in-memory SVG file.
parseSvgFile ::
  -- | Source path/URL of the document, used
  -- to resolve relative links.
  FilePath ->
  T.Text ->
  Maybe Document
parseSvgFile filename fileContent =
  parseXMLDoc fileContent >>= unparseDocument filename

parseSvg :: T.Text -> Tree
parseSvg inp =
  case parseXMLDoc inp of
    Nothing -> error "Invalid XML"
    Just xml -> unparse xml

-- | Save an SVG 'Document' to a file on disk.
saveXmlFile :: FilePath -> Document -> IO ()
saveXmlFile filePath =
  writeFile filePath . ppcTopElement prettyConfigPP . xmlOfDocument

cssDeclApplyer ::
  DrawAttributes ->
  CssDeclaration ->
  DrawAttributes
cssDeclApplyer value (CssDeclaration txt elems) =
  case lookup txt cssUpdaters of
    Nothing -> value
    Just f -> f value elems
  where
    cssUpdaters =
      [ (T.pack $ _attributeName n, u)
        | (n, u) <- drawAttributesList
      ]

-- | Rewrite a SVG 'Tree' using some CSS rules.
--
-- This action will propagate the definition of the
-- CSS directly in each matched element.
cssApply :: [CssRule] -> Tree -> Tree
cssApply rules = zipTree go
  where
    go [] = defaultSvg
    go ([] : _) = defaultSvg
    go context@((t : _) : _) = t & drawAttributes .~ attr'
      where
        matchingDeclarations =
          findMatchingDeclarations rules context
        attr = view drawAttributes t
        attr' = foldl' cssDeclApplyer attr matchingDeclarations

-- For every @\<use\>@ SVG tag, try to resolve the geometry associated
-- with it and place it in the scene 'Tree'. It is important to
-- resolve the @\<use\>@ tag before applying the CSS rules, as some
-- rules may apply some elements matching the children of "use".
-- resolveUses :: Document -> Document
-- resolveUses doc =
--   doc { _elements = mapTree fetchUses <$> _elements doc }
--   where
--     fetchUses (UseTree useInfo _) = UseTree useInfo $ search useInfo
--     fetchUses a                   = a
--
--     search nfo = M.lookup (_useName nfo) $ _definitions doc

-- -- | Rewrite the document by applying the CSS rules embedded
-- -- inside it.
-- applyCSSRules :: Document -> Document
-- applyCSSRules doc = doc
--     { _elements = cssApply (_styleRules doc) <$> _elements doc }
