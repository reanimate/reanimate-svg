module Graphics.SvgTree.Parser.ParseableContent where

import Graphics.SvgTree.Types.Contents
import Graphics.SvgTree.Parser.ContentsParser

import Data.Attoparsec.Text (Parser, parseOnly)
import qualified Data.Text as T


class ParseableContent a where
  parseContent :: T.Text -> Maybe a


parseC :: Parser a -> T.Text -> Maybe a
parseC p t = getRight $ parseOnly p t
  where
    getRight (Left _) = Nothing
    getRight (Right r) = Just r


-- <angle>
instance ParseableContent Angle where
  parseContent = parseC angleParser

-- <anything>
instance ParseableContent Anything where
  parseContent = parseC anythingParser

-- <basic-shape>
-- TODO

-- <clock-value>
-- TODO

-- <color>
instance ParseableContent Color where
  parseContent = parseC colorParser

-- <coordinate>
instance ParseableContent Coordinate where
  parseContent = parseC coordinateParser

-- <frequency>
instance ParseableContent Frequency where
  parseContent = parseC frequencyParser

-- <funcIRI>
instance ParseableContent FuncIRI where
  parseContent = parseC funcIRIParser

-- <ICCColor>
instance ParseableContent ICCColor where
  parseContent = parseC iccColorParser

-- <integer>
instance ParseableContent SVGInteger where
  parseContent = parseC integerParser

-- <IRI>
instance ParseableContent IRI where
  parseContent = parseC iriParser

-- <length>
instance ParseableContent Length where
  parseContent = parseC lengthParser

-- <length-percentage>
-- TODO

-- <list-of-Ts>
-- TODO

-- <name>
instance ParseableContent Name where
  parseContent = parseC nameParser

-- <number>
instance ParseableContent Number where
  parseContent = parseC numberParser

-- <number-optional-number>
-- TODO

-- <opacity-value>
instance ParseableContent OpacityValue where
  parseContent = parseC opacityValueParser

-- <paint>
-- TODO

-- <percentage>
instance ParseableContent Percentage where
  parseContent = parseC percentageParser

-- <time>
-- TODO

-- <transform-list>
-- TODO

-- <units>
instance ParseableContent Units where
  parseContent = parseC unitsParser

-- <URL>
instance ParseableContent URL where
  parseContent = parseC urlParser
