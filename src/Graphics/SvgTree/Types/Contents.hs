{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans -O0 #-}

-- | Define the types used to describes CSS elements
module Graphics.SvgTree.Types.Contents where

import           Graphics.SvgTree.Misc

import           Data.Hashable          (Hashable)
import           Data.Word              (Word8)
import           GHC.Generics           (Generic)
import           Text.Printf
import           Codec.Picture          (PixelRGBA8 (..))
import qualified Data.Text              as T



-- <angle>
data Angle
  = Deg Double       -- ^ Degrees
  | Grad Double      -- ^ Grads
  | Rad Double       -- ^ Radians
  | Angle Double     -- ^ No unit given, assumed to be in degrees.
  deriving (Eq, Show, Generic, Hashable)


mapAngle :: (Double -> Double) -> Angle -> Angle
mapAngle f a = case a of
  Deg n   -> Deg $ f n
  Grad n  -> Grad $ f n
  Rad n   -> Rad $ f n
  Angle n -> Angle $ f n


-- <anything>
newtype Anything = Anything T.Text
  deriving (Eq, Show, Generic, Hashable)


-- <basic-shape>
-- This is referenced in the clip-path attribute
-- https://developer.mozilla.org/en-US/docs/Web/CSS/basic-shape
-- TODO: Not complete, the type contructors should carry additional information.
data BasicShape
  = BasicShapeInset
  | BasicShapeCircle
  | BasicShapeEllipse
  | BasicShapePolygon
  | BasicShapePath
  | BasicShape
  deriving (Eq, Show, Generic, Hashable)


-- <clock-value>
-- Quite naive implementation, maybe better to look for a time library which could provide more functionality.
data ClockValue
  = FullClockVal Int Int Int Fraction
  | PartialClockVal Int Int Fraction
  | TimecountVal Int Fraction (Maybe Metric)
  deriving (Eq, Show, Generic, Hashable)

type Fraction = Maybe Int

data Metric
  = MetricHour
  | MetricMin
  | MetricSec
  | MetricMilisec
  deriving (Eq, Show, Generic, Hashable)


-- <color>
newtype Color = Color PixelRGBA8
  deriving (Eq, Show, Generic, Hashable)

deriving instance Generic PixelRGBA8
deriving instance Hashable PixelRGBA8

mkColor :: Word8 -> Word8 -> Word8 -> Color
mkColor r g b = Color $ PixelRGBA8 r g b 255

-- <coordinate>
data Coordinate
  = CoordinateEm Double        -- ^ The font-size of the relevant font
  | CoordinateEx Double        -- ^ The x-height of the relevant font.
  | CoordinatePx Double        -- ^ Pixels, relative to the viewing device.
  | CoordinateInches Double    -- ^ Inches.
  | CoordinateCm Double        -- ^ Centimeters
  | CoordinateMm Double        -- ^ Milimeters
  | CoordinatePt Double        -- ^ Points. The points used by CSS2 are equal to 1/72th of an inch.
  | CoordinatePc Double        -- ^ Picas. 1 pica is equal to 12 points.
  | CoordinatePercent Double   -- ^ Percententage
--  | Point Double     -- ^ Number in points, relative to DPI.
  | Coordinate Double    -- Adimensional = distance in the current user coordinate system.
  deriving (Eq, Show, Generic, Hashable)

-- | Helper function to modify inner value of a length
mapCoordinate :: (Double -> Double) -> Coordinate -> Coordinate
mapCoordinate f l = case l of
  CoordinateEm n      -> CoordinateEm $ f n
  CoordinateEx n      -> CoordinateEx $ f n
  CoordinatePx n      -> CoordinatePx $ f n
  CoordinateInches n  -> CoordinateInches $ f n
  CoordinateCm n      -> CoordinateCm $ f n
  CoordinateMm n      -> CoordinateMm $ f n
  CoordinatePt n      -> CoordinatePt $ f n
  CoordinatePc n      -> CoordinatePc $ f n
  CoordinatePercent n -> CoordinatePercent $ f n
  Coordinate n        -> Coordinate $ f n


-- <frequency>
data Frequency
  = Hz Double        -- ^ Hertz
  | KHz Double       -- ^ kilo Hertz
  deriving (Eq, Show, Generic, Hashable)

mapFrequency :: (Double -> Double) -> Frequency -> Frequency
mapFrequency f freq = case freq of
  Hz  n -> Hz $ f n
  KHz n -> KHz $ f n


-- <funcIRI>
newtype FuncIRI = FuncIRI T.Text
  deriving (Eq, Show, Generic, Hashable)


-- <ICCColor>
newtype ICCColor = ICCColor T.Text
  deriving (Eq, Show, Generic, Hashable)


-- <integer>
newtype SVGInteger = SVGInteger Int
  deriving (Eq, Show, Generic, Hashable)

mapInteger :: (Int -> Int) -> SVGInteger -> SVGInteger
mapInteger f (SVGInteger i) = SVGInteger $ f i


-- <IRI>                      -- TODO: separate absolute IRI from relative IRI?
newtype IRI = IRI T.Text
  deriving (Eq, Show, Generic, Hashable)


-- <Length>
data Length
  = Em Double        -- ^ The font-size of the relevant font
  | Ex Double        -- ^ The x-height of the relevant font.
  | Px Double        -- ^ Pixels, relative to the viewing device.
  | Inches Double    -- ^ Inches.
  | Cm Double        -- ^ Centimeters
  | Mm Double        -- ^ Milimeters
  | Pt Double        -- ^ Points. The points used by CSS2 are equal to 1/72th of an inch.
  | Pc Double        -- ^ Picas. 1 pica is equal to 12 points.
  | Percent Double   -- ^ Percententage
--  | Point Double     -- ^ Number in points, relative to DPI.
  | Length Double    -- Adimensional = distance in the current user coordinate system.
  deriving (Eq, Show, Generic, Hashable)

-- | Helper function to modify inner value of a length
mapLength :: (Double -> Double) -> Length -> Length
mapLength f l = case l of
  Em n      -> Em $ f n
  Ex n      -> Ex $ f n
  Px n      -> Px $ f n
  Inches n  -> Inches $ f n
  Cm n      -> Cm $ f n
  Mm n      -> Mm $ f n
  Pt n      -> Pt $ f n
  Pc n      -> Pc $ f n
  Percent n -> Percent $ f n
  Length n  -> Length $ f n


-- <length-percentage>
-- newtype LengthPercentage = LengthPercentage (Either Length Percentage)
--   deriving (Eq, Show, Generic, Hashable)

-- instance SerializableAttribute LengthPercentage where
--   serializeAttribute (LengthPercentage n) = case n of
--     Left l  -> serializeAttribute l
--     Right p -> serializeAttribute p


-- <list-of-Ts>
newtype ListOfTs a = ListOfTs [a]
  deriving (Eq, Show, Generic, Hashable)


-- <name>
newtype Name = Name T.Text
  deriving (Eq, Show, Generic, Hashable)


-- <number>
-- | Encode complex number possibly dependant to the current
-- render size.
newtype Number = Num Double       -- ^ Simple coordinate in current user coordinate.
  deriving (Eq, Show, Generic, Hashable)

-- | Helper function to modify inner value of a number
mapNumber :: (Double -> Double) -> Number -> Number
mapNumber f (Num n) = Num $ f n


-- <number-optional-number
data NumberOptionalNumber = NumOpt Number (Maybe Number)
  deriving (Eq, Show, Generic, Hashable)


-- <opacity-value>
newtype OpacityValue = OpacityValue Double
  deriving (Eq, Show, Generic, Hashable)

mapOpacityValue :: (Double -> Double) -> OpacityValue -> OpacityValue
mapOpacityValue f (OpacityValue n) = OpacityValue $ f n


-- <paint>
data Paint
  = PaintNone
  | PaintColor Color
  | PaintURL URL (Maybe Color)                       --TODO: change to <URL>
  | PaintContextFill
  | PaintContextStroke
  deriving (Eq, Show, Generic, Hashable)


-- <percentage>
newtype Percentage = Percentage Double
  deriving (Eq, Show, Generic, Hashable)

mapPercentage :: (Double -> Double) -> Percentage -> Percentage
mapPercentage f (Percentage p) = Percentage $ f p


-- <time>
data Time
  = Ms Double        -- ^ miliseconds
  | Sec Double       -- ^ seconds
  deriving (Eq, Show, Generic, Hashable)

mapTime :: (Double -> Double) -> Time -> Time
mapTime f t = case t of
  Ms  n -> Ms  $ f n
  Sec n -> Sec $ f n


-- <transform-list>
data TransformFunction
  = Matrix Number Number Number Number Number Number
  | Translate Number (Maybe Number)
  | Scale Number (Maybe Number)
  | Rotate Number (Maybe (Number, Number))
  | SkewX Number
  | SkewY Number
  | TransformUnknown
  deriving (Eq, Show, Generic, Hashable)

newtype TransformList = TransformList [TransformFunction]
  deriving (Eq, Show, Generic, Hashable)


-- <units>
-- Not part of the standard, but useful in many attributes definitions.
data Units
  = UserSpaceOnUse
  | ObjectBoundingBox
  deriving (Eq, Show, Generic, Hashable)


-- <URL>
newtype URL = URL T.Text
  deriving (Eq, Show, Generic, Hashable)
