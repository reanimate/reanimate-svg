{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans -O0 #-}

-- | Define the types used to describes CSS elements
module Graphics.SvgTree.Types.Contents where

import           Data.Hashable          (Hashable)
import           Data.Word              (Word8)
import           GHC.Generics           (Generic)
import           Text.Printf
import           Codec.Picture          (PixelRGBA8 (..))
import qualified Data.Text              as T

import           Graphics.SvgTree.Misc


class SerializableAttribute a where      -- Change to SerializableContent?
  serializeAttribute :: a -> String      -- Change to Maybe String?


-- <angle>
data Angle
  = Deg Double       -- ^ Degrees
  | Grad Double      -- ^ Grads
  | Rad Double       -- ^ Radians
  | Angle Double     -- ^ No unit given, assumed to be in degrees.
  deriving (Eq, Show, Generic, Hashable)

instance SerializableAttribute Angle where
  serializeAttribute a = case a of
    Deg n   -> printf "%sdeg"  (ppD n)
    Grad n  -> printf "%sgrad" (ppD n)
    Rad n   -> printf "%srad"  (ppD n)
    Angle n -> printf "%s"     (ppD n)

mapAngle :: (Double -> Double) -> Angle -> Angle
mapAngle f a = case a of
  Deg n   -> Deg $ f n
  Grad n  -> Grad $ f n
  Rad n   -> Rad $ f n
  Angle n -> Angle $ f n


-- <anything>
newtype Anything = Anything T.Text
  deriving (Eq, Show, Generic, Hashable)

instance SerializableAttribute Anything where
  serializeAttribute (Anything s) = T.unpack s


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
  = FullClockVal Int Int Int (Maybe Int)
  | PartialClockVal Int Int (Maybe Int)
  | TimecountVal Int (Maybe Int) (Maybe Metric)
  deriving (Eq, Show, Generic, Hashable)

data Metric
  = MetricHour
  | MetricMin
  | MetricSec
  | MetricMilisec
  deriving (Eq, Show, Generic, Hashable)

instance SerializableAttribute Metric where
  serializeAttribute MetricHour = "h"
  serializeAttribute MetricMin  = "min"
  serializeAttribute MetricSec  = "s"
  serializeAttribute MetricMilisec = "ms"

instance SerializableAttribute ClockValue where
  serializeAttribute cv = case cv of
    FullClockVal h m s f ->
      printf "%d:%d:%d" (ppI h) (ppI m) (ppI s) ++ (serializeFraction f)
    PartialClockVal m s f ->
      printf "%d:%d" (ppI m) (ppI s) ++ (serializeFraction f)
    TimecountVal t f m ->
      printf "%d" (ppI t) ++ (serializeFraction f) ++ (serializeMetric m)
    where
      serializeFraction Nothing = ""
      serializeFraction (Just i) = "." ++ show i
      serializeMetric Nothing = ""
      serializeMetric (Just x) = serializeAttribute x


-- <color>
newtype Color = Color PixelRGBA8
  deriving (Eq, Show, Generic, Hashable)

deriving instance Generic PixelRGBA8
deriving instance Hashable PixelRGBA8

instance SerializableAttribute Color where
  serializeAttribute (Color (PixelRGBA8 r g b _)) =
    printf "#%02X%02X%02X" r g b

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

-- | Encode the number to string which can be used in a
-- CSS or a svg attributes.
instance SerializableAttribute Coordinate where
  serializeAttribute n = case n of
    CoordinateEm cc     -> printf "%sem" (ppD cc)
    CoordinateEx l      -> printf "%sex" (ppD l)
    CoordinatePx c      -> printf "%spx" (ppD c)
    CoordinateInches i  -> printf "%sin" (ppD i)
    CoordinateCm c      -> printf "%scm" (ppD c)
    CoordinateMm m      -> printf "%smm" (ppD m)
    CoordinatePt p      -> printf "%spt" (ppD p)
    CoordinatePc p      -> printf "%spc" (ppD p)
    CoordinatePercent p -> printf "%s%%" (ppD p)
    Coordinate l        -> printf "%s"   (ppD l)

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

instance SerializableAttribute Frequency where
  serializeAttribute freq = case freq of
    Hz  n -> printf "%sHz"  (ppD n)
    KHz n -> printf "%skHz" (ppD n)

mapFrequency :: (Double -> Double) -> Frequency -> Frequency
mapFrequency f freq = case freq of
  Hz  n -> Hz $ f n
  KHz n -> KHz $ f n


-- <funcIRI>
newtype FuncIRI = FuncIRI T.Text
  deriving (Eq, Show, Generic, Hashable)

instance SerializableAttribute FuncIRI where
  serializeAttribute (FuncIRI s) = "url(" ++ T.unpack s ++ ")"


-- <ICCColor>
newtype ICCColor = ICCColor T.Text
  deriving (Eq, Show, Generic, Hashable)

instance SerializableAttribute ICCColor where
  serializeAttribute (ICCColor s) = "icc-color(" ++ (T.unpack s) ++ ")"


-- <integer>
newtype SVGInteger = SVGInteger Int
  deriving (Eq, Show, Generic, Hashable)

instance SerializableAttribute SVGInteger where
  serializeAttribute (SVGInteger i) = ppI i

mapInteger :: (Int -> Int) -> SVGInteger -> SVGInteger
mapInteger f (SVGInteger i) = SVGInteger $ f i


-- <IRI>                      -- TODO: separate absolute IRI from relative IRI?
newtype IRI = IRI T.Text
  deriving (Eq, Show, Generic, Hashable)

instance SerializableAttribute IRI where
  serializeAttribute (IRI s) = T.unpack s

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

instance SerializableAttribute Length where
-- | Encode the number to string which can be used in a
-- CSS or a svg attributes.
  serializeAttribute n = case n of
    Em cc     -> printf "%sem" (ppD cc)
    Ex l      -> printf "%sex" (ppD l)
    Px c      -> printf "%spx" (ppD c)
    Inches i  -> printf "%sin" (ppD i)
    Cm c      -> printf "%scm" (ppD c)
    Mm m      -> printf "%smm" (ppD m)
    Pt p      -> printf "%spt" (ppD p)
    Pc p      -> printf "%spc" (ppD p)
    Percent p -> printf "%s%%" (ppD p)
    Length l  -> printf "%s"   (ppD l)

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

instance (SerializableAttribute a) => SerializableAttribute (ListOfTs a) where
  serializeAttribute (ListOfTs l) = unwords . fmap serializeAttribute $ l


-- <name>
newtype Name = Name T.Text
  deriving (Eq, Show, Generic, Hashable)

instance SerializableAttribute Name where
  serializeAttribute (Name s) = T.unpack s


-- <number>
-- | Encode complex number possibly dependant to the current
-- render size.
newtype Number = Num Double       -- ^ Simple coordinate in current user coordinate.
  deriving (Eq, Show, Generic, Hashable)

instance SerializableAttribute Number where
-- | Encode the number to string which can be used in a
-- CSS or a svg attributes.
  serializeAttribute (Num n) = ppD n

-- | Helper function to modify inner value of a number
mapNumber :: (Double -> Double) -> Number -> Number
mapNumber f (Num n) = Num $ f n


-- <number-optional-number
data NumberOptionalNumber = NumOpt Number (Maybe Number)
  deriving (Eq, Show, Generic, Hashable)

instance SerializableAttribute NumberOptionalNumber where
  serializeAttribute n = case n of
    NumOpt a Nothing  -> serializeAttribute a
    NumOpt a (Just b) ->
      printf "%s, %s" (serializeAttribute a) (serializeAttribute b)


-- <opacity-value>
newtype OpacityValue = OpacityValue Double
  deriving (Eq, Show, Generic, Hashable)

instance SerializableAttribute OpacityValue where
  serializeAttribute (OpacityValue n) = ppD n

mapOpacityValue :: (Double -> Double) -> OpacityValue -> OpacityValue
mapOpacityValue f (OpacityValue n) = OpacityValue $ f n


-- <paint>
data Paint
  = PaintNone
  | PaintColor Color
  | PaintURL String (Maybe Color)                       --TODO: change to <URL>
  | PaintContextFill
  | PaintContextStroke
  deriving (Eq, Show, Generic, Hashable)

instance SerializableAttribute Paint where
  serializeAttribute p = case p of
    PaintNone           -> "none"
    PaintColor c        -> serializeAttribute c
    PaintURL s Nothing  -> s
    PaintURL s (Just c) -> s ++ (serializeAttribute c)
    PaintContextFill    -> "context-fill"
    PaintContextStroke  -> "context-stroke"


-- <percentage>
newtype Percentage = Percentage Double
  deriving (Eq, Show, Generic, Hashable)

instance SerializableAttribute Percentage where
  serializeAttribute (Percentage p) = printf "%s%%" (ppD p)

mapPercentage :: (Double -> Double) -> Percentage -> Percentage
mapPercentage f (Percentage p) = Percentage $ f p


-- <time>
data Time
  = Ms Double        -- ^ miliseconds
  | Sec Double       -- ^ seconds
  deriving (Eq, Show, Generic, Hashable)

instance SerializableAttribute Time where
  serializeAttribute t = case t of
    Ms  n -> printf "%sms" (ppD n)
    Sec n -> printf "%ss"  (ppD n)

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

instance SerializableAttribute TransformFunction where
  serializeAttribute t = case t of
    Matrix a b c d e f ->
      printf
        "matrix(%s, %s, %s, %s, %s, %s)"
        (serializeAttribute a)
        (serializeAttribute b)
        (serializeAttribute c)
        (serializeAttribute d)
        (serializeAttribute e)
        (serializeAttribute f)
    Translate x (Nothing) ->
      printf "translate(%s)" (serializeAttribute x)
    Translate x (Just y)  ->
      printf "translate(%s, %s)" (serializeAttribute x) (serializeAttribute y)
    Scale x Nothing       ->
      printf "scale(%s)" (serializeAttribute x)
    Scale x (Just y)      ->
      printf "scale(%s, %s)" (serializeAttribute x) (serializeAttribute y)
    Rotate angle Nothing  ->
      printf "rotate(%s)" (serializeAttribute angle)
    Rotate angle (Just (x, y)) ->
      printf
        "rotate(%s, %s, %s)"
        (serializeAttribute angle)
        (serializeAttribute x)
        (serializeAttribute y)
    SkewX x ->
      printf "skewX(%s)" (serializeAttribute x)
    SkewY y ->
      printf "skewY(%s)" (serializeAttribute y)
    TransformUnknown -> ""

newtype TransformList = TransformList [TransformFunction]
  deriving (Eq, Show, Generic, Hashable)

instance SerializableAttribute TransformList where
  serializeAttribute (TransformList l) =
    unwords $ (fmap serializeAttribute) l


-- <units>
-- Not part of the standard, but useful in many attributes definitions.
data Units
  = UserSpaceOnUse
  | ObjectBoundingBox
  deriving (Eq, Show, Generic, Hashable)

instance SerializableAttribute Units where
  serializeAttribute u = case u of
    UserSpaceOnUse    -> "userSpaceOnUse"
    ObjectBoundingBox -> "objectBoundingBox"

-- <URL>
newtype URL = URL T.Text
  deriving (Eq, Show, Generic, Hashable)

instance SerializableAttribute URL where
  serializeAttribute (URL s) = T.unpack s
