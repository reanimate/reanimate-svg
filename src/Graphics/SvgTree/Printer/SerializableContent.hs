

module Graphics.SvgTree.Printer.SerializableContent where

import Graphics.SvgTree.Types.Contents
import Graphics.SvgTree.Printer.ContentsPrinter

import qualified Data.Text as T


class SerializableContent a where
  serializeContent :: a -> T.Text


-- <angle>
instance SerializableContent Angle where
  serializeContent = printAngle

-- <anything>
instance SerializableContent Anything where
  serializeContent = printAnything

-- <basic-shape>
-- TODO

-- <clock-value>
instance SerializableContent ClockValue where
  serializeContent = printClockValue

-- <color>
instance SerializableContent Color where
  serializeContent = printColor

-- <coordinate>
instance SerializableContent Coordinate where
  serializeContent = printCoordinate

-- <frequency>
instance SerializableContent Frequency where
  serializeContent = printFrequency

-- <funcIRI>
instance SerializableContent FuncIRI where
  serializeContent = printFuncIRI

-- <ICCColor>
instance SerializableContent ICCColor where
  serializeContent = printICCColor

-- <integer>
instance SerializableContent SVGInteger where
  serializeContent = printInteger

-- <IRI>
instance SerializableContent IRI where
  serializeContent = printIRI

-- <length>
instance SerializableContent Length where
  serializeContent = printLength

-- <list-of-Ts>
instance (SerializableContent a) => SerializableContent (ListOfTs a) where
  serializeContent (ListOfTs l) = T.unwords . fmap serializeContent $ l

-- <name>
instance SerializableContent Name where
  serializeContent = printName

-- <number>
instance SerializableContent Number where
  serializeContent = printNumber

-- <number-optional-number>
instance SerializableContent NumberOptionalNumber where
  serializeContent = printNumberOptionalNumber

-- <opacity-value>
instance SerializableContent OpacityValue where
  serializeContent = printOpacityValue

-- <paint>
instance SerializableContent Paint where
  serializeContent = printPaint

-- <time>
instance SerializableContent Time where
  serializeContent = printTime

-- <transform-list>
instance SerializableContent TransformList where
  serializeContent = printTransformList

-- <units>
instance SerializableContent Units where
  serializeContent = printUnits

-- <URL>
instance SerializableContent URL where
  serializeContent = printURL
