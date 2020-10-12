{-# LANGUAGE OverloadedStrings #-}

module Graphics.SvgTree.Parser.ContentsParser where

import Graphics.SvgTree.Types
import Graphics.SvgTree.Types.Contents

import           Data.Scientific            (toRealFloat)
import qualified Data.Text                  as T
import           Control.Applicative        ((<|>))

import           Data.Attoparsec.Text (Parser, char, digit, many1,
                                       anyChar, signed, decimal,
                                       parseOnly, scientific, skipSpace,
                                       string, choice, takeText, manyTill)


doubleParser :: Parser Double
doubleParser = negate <$ string "-" <*> doubleNumber
            <|> string "+" *> doubleNumber
            <|> doubleNumber
  where
    doubleNumber :: Parser Double
    doubleNumber = toRealFloat <$> scientific <|> shorthand

    shorthand = process' <$> (string "." *> many1 digit)
    process' = either (const 0) id . parseOnly doubleNumber . T.pack . (++) "0."


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
-- TODO

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
parseFuncIRI :: Parser FuncIRI
parseFuncIRI = FuncIRI . T.pack <$> ("url" *> manyTill anyChar (char ')'))

-- <ICCColor>
-- TODO

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
