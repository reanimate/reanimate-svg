{-# LANGUAGE OverloadedStrings #-}

module Graphics.SvgTree.ColorParser
  ( colorParser,
    colorSerializer,
    textureParser,
    textureSerializer,
    urlRef,
  )
where

import Codec.Picture (PixelRGBA8 (..))
import Control.Applicative ((<|>))
import Data.Attoparsec.Text
  ( Parser,
    char,
    digit,
    inClass,
    letter,
    many1,
    option,
    satisfy,
    scientific,
    skipSpace,
    string,
    takeWhile1,
  )
import Data.Bits (unsafeShiftL, (.|.))
import Data.Functor (($>))
import qualified Data.Map as M
import Data.Scientific (toRealFloat)
import Data.Word (Word8)
import Graphics.SvgTree.NamedColors (svgNamedColors)
import Graphics.SvgTree.Types (Texture (..))
import Text.Printf (printf)

commaWsp :: Parser ()
commaWsp =
  skipSpace *> option () (string "," $> ())
    <* skipSpace

num :: Parser Double
num = realToFrac <$> (skipSpace *> plusMinus <* skipSpace)
  where
    doubleNumber :: Parser Double
    doubleNumber = toRealFloat <$> scientific

    plusMinus =
      negate <$ string "-" <*> doubleNumber
        <|> string "+" *> doubleNumber
        <|> doubleNumber

colorSerializer :: PixelRGBA8 -> String
colorSerializer (PixelRGBA8 r g b _) = printf "#%02X%02X%02X" r g b

colorParser :: Parser PixelRGBA8
colorParser =
  rgbColor
    <|> (string "#" *> (color <|> colorReduced))
    <|> namedColor
  where
    charRange c1 c2 =
      (\c -> fromIntegral $ fromEnum c - fromEnum c1) <$> satisfy (\v -> c1 <= v && v <= c2)

    black = PixelRGBA8 0 0 0 255 -- FIXME: Throw exception?
    namedColor = do
      str <- takeWhile1 (inClass "a-z")
      return $ M.findWithDefault black str svgNamedColors

    hexChar :: Parser Word8
    hexChar =
      charRange '0' '9'
        <|> ((+ 10) <$> charRange 'a' 'f')
        <|> ((+ 10) <$> charRange 'A' 'F')

    percentToWord v = floor $ v * (255 / 100)

    numPercent =
      ((percentToWord <$> num) <* string "%")
        <|> (floor <$> num)

    hexByte =
      (\h1 h2 -> h1 `unsafeShiftL` 4 .|. h2)
        <$> hexChar <*> hexChar

    color =
      (\r g b -> PixelRGBA8 r g b 255)
        <$> hexByte <*> hexByte <*> hexByte
    rgbColor =
      (\r g b -> PixelRGBA8 r g b 255)
        <$> (string "rgb(" *> numPercent)
        <*> (commaWsp *> numPercent)
        <*> (commaWsp *> numPercent <* skipSpace <* string ")")

    colorReduced =
      (\r g b -> PixelRGBA8 (r * 17) (g * 17) (b * 17) 255)
        <$> hexChar <*> hexChar <*> hexChar

textureSerializer :: Texture -> String
textureSerializer (ColorRef px) = colorSerializer px
textureSerializer (TextureRef str) = printf "url(#%s)" str
textureSerializer FillNone = "none"

urlRef :: Parser String
urlRef =
  string "url(" *> skipSpace
    *> char '#'
    *> many1 (letter <|> digit <|> char '_' <|> char '.' <|> char '-' <|> char ':')
    <* skipSpace
    <* char ')'
    <* skipSpace

textureParser :: Parser Texture
textureParser =
  none <|> (TextureRef <$> urlRef)
    <|> (ColorRef <$> colorParser)
  where
    none = FillNone <$ string "none"
