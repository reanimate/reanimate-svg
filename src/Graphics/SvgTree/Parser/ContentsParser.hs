{-# LANGUAGE OverloadedStrings #-}

module Graphics.SvgTree.Parser.ContentsParser where

import Graphics.SvgTree.Types
import Graphics.SvgTree.Types.Contents
import Graphics.SvgTree.NamedColors

import           Codec.Picture          (PixelRGBA8 (..))
import           Data.Scientific        (toRealFloat)
import qualified Data.Text              as T
import           Control.Applicative    ((<|>))
import           Data.Word              (Word8)
import qualified Data.Map               as M
import           Data.Bits              (unsafeShiftL, (.|.))
import           Data.Functor


import           Data.Attoparsec.Text   hiding (Number)
                 -- (Parser, char, digit, many1,
                                        --  anyChar, signed, decimal,
                                        --  parseOnly, scientific, skipSpace,
                                        --  string, choice, takeText, manyTill)


doubleParser :: Parser Double
doubleParser = negate <$ string "-" <*> doubleNumber
            <|> string "+" *> doubleNumber
            <|> doubleNumber
  where
    doubleNumber :: Parser Double
    doubleNumber = toRealFloat <$> scientific <|> shorthand

    shorthand = process' <$> (string "." *> many1 digit)
    process' = either (const 0) id . parseOnly doubleNumber . T.pack . (++) "0."

commaWsp :: Parser ()
commaWsp = skipSpace *> option () ("," $> ())
                     <* skipSpace

-- <angle>
angleParser :: Parser Angle
angleParser = choice
              [ Deg   <$> (skipSpace *> doubleParser <* string "deg")
              , Grad  <$> (skipSpace *> doubleParser <* string "grad")
              , Rad   <$> (skipSpace *> doubleParser <* string "rad")
              , Angle <$> (skipSpace *> doubleParser <* skipSpace)]

-- <anything>
anythingParser :: Parser Anything
anythingParser = Anything <$> takeText

-- <basic-shape>
-- TODO

-- <clock-value>
-- TODO

-- <color>
colorParser :: Parser Color --PixelRGBA8
colorParser = Color  <$> rgbColor
          <|> Color  <$> (string "#" *> (color <|> colorReduced))
          <|> Color  <$> namedColor
  where
    charRange c1 c2 =
        (\c -> fromIntegral $ fromEnum c - fromEnum c1) <$> satisfy (\v -> c1 <= v && v <= c2)
    black = PixelRGBA8 0 0 0 255

    hexChar :: Parser Word8
    hexChar = charRange '0' '9'
           <|> ((+ 10) <$> charRange 'a' 'f')
           <|> ((+ 10) <$> charRange 'A' 'F')

    namedColor = do
      str <- takeWhile1 (inClass "a-zA-Z")
      return $ M.findWithDefault black (T.toLower str) svgNamedColors

    percentToWord v = floor $ v * (255 / 100)

    numPercent = ((percentToWord <$> doubleParser) <* string "%")
              <|> (floor <$> doubleParser)

    hexByte = (\h1 h2 -> h1 `unsafeShiftL` 4 .|. h2)
           <$> hexChar <*> hexChar

    color = (\r g b -> PixelRGBA8 r g b 255)
         <$> hexByte <*> hexByte <*> hexByte
    rgbColor = (\r g b -> PixelRGBA8 r g b 255)
            <$> (asciiCI "rgb(" *> numPercent)
            <*> (commaWsp *> numPercent)
            <*> (commaWsp *> numPercent <* skipSpace <* string ")")

    colorReduced =
        (\r g b -> PixelRGBA8 (r * 17) (g * 17) (b * 17) 255)
        <$> hexChar <*> hexChar <*> hexChar


-- <coordinate>
coordinateParser :: Parser Coordinate
coordinateParser = choice
                   [ CoordinateEm <$> (skipSpace *> doubleParser <* string "em")
                   , CoordinateEx <$> (skipSpace *> doubleParser <* string "ex")
                   , CoordinatePx <$> (skipSpace *> doubleParser <* string "px")
                   , CoordinateInches <$> (skipSpace *> doubleParser <* string "in")
                   , CoordinateCm <$> (skipSpace *> doubleParser <* string "cm")
                   , CoordinateMm <$> (skipSpace *> doubleParser <* string "mm")
                   , CoordinatePt <$> (skipSpace *> doubleParser <* string "pt")
                   , CoordinatePc <$> (skipSpace *> doubleParser <* string "pc")
                   , CoordinatePercent <$> (skipSpace *> doubleParser <* string "%")
                   , Coordinate   <$> (skipSpace *> doubleParser <* skipSpace)]

-- <frequency>
frequencyParser :: Parser Frequency
frequencyParser = choice
                  [ Hz <$> (skipSpace *> doubleParser <* string "Hz")
                  , KHz <$> (skipSpace *> doubleParser <* string "kHz")]

-- <funcIRI>
funcIRIParser :: Parser FuncIRI
funcIRIParser = FuncIRI . T.pack <$> ("url" *> manyTill anyChar (char ')'))

-- <ICCColor>
-- TODO: not correct.
-- icccolor ::= "icc-color(" name (, number)+ ")"
iccColorParser :: Parser ICCColor
iccColorParser = ICCColor . T.pack <$> ("icc-color(" *> manyTill anyChar (char ')'))

-- <integer>
integerParser :: Parser SVGInteger
integerParser = SVGInteger <$> signed decimal

-- <IRI>
iriParser :: Parser IRI
iriParser = IRI <$> takeText

-- <Length>
lengthParser :: Parser Length
lengthParser = choice
                   [ Em <$> (skipSpace *> doubleParser <* string "em")
                   , Ex <$> (skipSpace *> doubleParser <* string "ex")
                   , Px <$> (skipSpace *> doubleParser <* string "px")
                   , Inches <$> (skipSpace *> doubleParser <* string "in")
                   , Cm <$> (skipSpace *> doubleParser <* string "cm")
                   , Mm <$> (skipSpace *> doubleParser <* string "mm")
                   , Pt <$> (skipSpace *> doubleParser <* string "pt")
                   , Pc <$> (skipSpace *> doubleParser <* string "pc")
                   , Percent <$> (skipSpace *> doubleParser <* string "%")
                   , Length  <$> (skipSpace *> doubleParser <* skipSpace)]

-- <length-percentage>
-- TODO

-- <list-of-Ts>
-- TODO

-- <name>
-- TODO: not correct.
-- name  ::= [^,()#x20#x9#xD#xA] /* any char except ",", "(", ")" or wsp */
nameParser :: Parser Name
nameParser = Name <$> takeText

-- <number>
numberParser :: Parser Number
numberParser = Num <$> (skipSpace *> doubleParser <* skipSpace)

-- <number-optional-number>
--TODO

-- <opacity-value>
opacityValueParser :: Parser OpacityValue
opacityValueParser = OpacityValue <$> (skipSpace *> doubleParser)

-- <paint>
-- TODO

-- <percentage>
percentageParser :: Parser Percentage
percentageParser = Percentage <$> (skipSpace *> doubleParser <* string "%")

-- <time>
-- TODO

-- <transform-list>
-- TODO

-- <units>
unitsParser :: Parser Units
unitsParser = choice
              [ UserSpaceOnUse <$ string "userSpaceOnUse"
              , ObjectBoundingBox <$ string "objectBoundingBox"]

-- <URL>
urlParser :: Parser URL
urlParser = URL <$> takeText
