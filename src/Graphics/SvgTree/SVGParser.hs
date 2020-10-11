{-# LANGUAGE ViewPatterns      #-}

module Graphics.SvgTree.SVGParser where

import Graphics.SvgTree.Types
import Graphics.SvgTree.Types.Elements
import Graphics.SvgTree.Types.Attributes

import qualified Text.XML.Light as X
import qualified Data.Text      as T
import           Data.Attoparsec.Text         (Parser, parseOnly, string)

nodeName :: X.Element -> String
nodeName = X.qName . X.elName

setName :: String -> X.Element -> X.Element
setName name elt = elt{ X.elName = X.unqual name }

attributeFinder :: String -> X.Element -> Maybe String
attributeFinder str =
    X.findAttrBy (\a -> X.qName a == str)

parse :: Parser a -> String -> Maybe a
parse p str = case parseOnly p (T.pack str) of
  Left _  -> Nothing
  Right r -> Just r

-- viewBoxParser :: Parser (Double, Double, Double, Double)
-- viewBoxParser = (,,,)
--        <$> iParse <*> iParse <*> iParse <*> iParse
--   where
--     iParse = num <* skipSpace

unparseSVG :: FilePath -> X.Element -> Maybe SVG
unparseSVG rootLocation e@(nodeName -> "svg")
  = Just SVG
  { _svgCoreAttributes = defaultSvg,
    _svgStylingAttributes = defaultSvg,
    _svgConditionalProcessingAttributes = defaultSvg,
    _svgPresentationAttributes = defaultSvg,
    _svgBaseProfile = Nothing,
    --    _svgContentScriptType = Nothing,
    --    _svgContentStyleType = Nothing,
    _svgHeight = Nothing,
    _svgWidth = Nothing,
    _svgPreserveAspectRatio = Nothing,
    _svgVersion = Nothing,
    _svgViewBox = Nothing,--attributeFinder "viewBox" e >>= parse viewBoxParser,
    _svgX = Nothing,
    _svgY = Nothing
  }
unparseSVG _ _ = Nothing
