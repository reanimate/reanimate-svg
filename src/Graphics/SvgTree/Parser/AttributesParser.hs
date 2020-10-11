{-# LANGUAGE OverloadedStrings #-}

module Graphics.SvgTree.Parser.AttributesParser where

import Graphics.SvgTree.Types
import Graphics.SvgTree.Types.Attributes
import Graphics.SvgTree.Types.Contents
import Graphics.SvgTree.Parser.ContentsParser

import           Data.Scientific            (toRealFloat)
import qualified Data.Text                  as T
import           Control.Applicative        ((<|>))

import           Data.Attoparsec.Text (Parser, char, digit, many1,
                                       parseOnly, scientific, skipSpace,
                                       string)

viewBoxParser :: Parser ViewBox
viewBoxParser =  viewBox
       <$> iParse <*> iParse <*> iParse <*> iParse
  where
    iParse = numberParser <* skipSpace
    viewBox a b c d = ViewBox $ Just (a,b,c,d)
