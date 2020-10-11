{-# LANGUAGE OverloadedStrings #-}

module Graphics.SvgTree.Parser.ContentsParser where

import Graphics.SvgTree.Types
import Graphics.SvgTree.Types.Contents

import           Data.Scientific            (toRealFloat)
import qualified Data.Text                  as T
import           Control.Applicative        ((<|>))

import           Data.Attoparsec.Text (Parser, char, digit, many1,
                                       parseOnly, scientific, skipSpace,
                                       string, choice)


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
-- <basic-shape>
-- <clock-value>
-- <color>
-- <coordinate>
-- <frequency>
-- <funcIRI>
-- <ICCColor>
-- <integer>
-- <IRI>
-- <Length>
-- <length-percentage>
-- <list-of-Ts>
-- <name>
-- <number>
numberParser :: Parser Number
numberParser = Num <$> (skipSpace *> doubleParser <* skipSpace)

-- <number-optional-number>
-- <opacity-value>
-- <paint>
-- <percentage>
-- <time>
-- <transform-list>
-- <units>
-- <URL>
