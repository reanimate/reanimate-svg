{-# LANGUAGE OverloadedStrings #-}

module Graphics.SvgTree.Printer.ContentsPrinter where

import Graphics.SvgTree.Types.Contents
import           Graphics.SvgTree.Misc

import           Data.Text
import           Text.Printf
import           Codec.Picture          (PixelRGBA8 (..))

-- <angle>
printAngle :: Angle -> Text
printAngle a = pack $ case a of
    Deg n   -> printf "%sdeg"  (ppD n)
    Grad n  -> printf "%sgrad" (ppD n)
    Rad n   -> printf "%srad"  (ppD n)
    Angle n -> printf "%s"     (ppD n)

-- <anything>
printAnything :: Anything -> Text
printAnything (Anything t) = t

-- <basic-shape>
-- TODO

-- <clock-value>
printClockValue :: ClockValue -> Text
printClockValue cv = pack $ case cv of
    FullClockVal h m s f ->
      printf "%02d:%02d:%02d" h m s ++ (printFraction f)
    PartialClockVal m s f ->
      printf "%02d:%02d" m s ++ (printFraction f)
    TimecountVal t f m ->
      printf "%d" t ++ (printFraction f) ++ (printMetric m)
    where
      printFraction :: Fraction -> String
      printFraction Nothing = ""
      printFraction (Just i) = "." ++ show i

      printMetric :: Maybe Metric -> String
      printMetric Nothing              = ""
      printMetric (Just MetricHour)    = "h"
      printMetric (Just MetricMin)     = "min"
      printMetric (Just MetricSec)     = "s"
      printMetric (Just MetricMilisec) = "ms"


-- <color>
printColor :: Color -> Text
printColor (Color (PixelRGBA8 r g b _)) = pack $ printf "#%02X%02X%02X" r g b


-- <coordinate>
printCoordinate :: Coordinate -> Text
printCoordinate c = pack $ case c of
    CoordinateEm cc     -> printf "%sem" (ppD cc)
    CoordinateEx l      -> printf "%sex" (ppD l)
    CoordinatePx cc     -> printf "%spx" (ppD cc)
    CoordinateInches i  -> printf "%sin" (ppD i)
    CoordinateCm cc     -> printf "%scm" (ppD cc)
    CoordinateMm m      -> printf "%smm" (ppD m)
    CoordinatePt p      -> printf "%spt" (ppD p)
    CoordinatePc p      -> printf "%spc" (ppD p)
    CoordinatePercent p -> printf "%s%%" (ppD p)
    Coordinate l        -> printf "%s"   (ppD l)


-- <frequency>
printFrequency :: Frequency -> Text
printFrequency freq = pack $ case freq of
    Hz  n -> printf "%sHz"  (ppD n)
    KHz n -> printf "%skHz" (ppD n)


-- <funcIRI>
printFuncIRI :: FuncIRI -> Text
printFuncIRI (FuncIRI s) = "url(" <> s <> ")"


-- <ICCColor>
printICCColor :: ICCColor -> Text
printICCColor (ICCColor s) = "icc-color(" <> s <> ")"


-- <integer>
printInteger :: SVGInteger -> Text
printInteger (SVGInteger i) = pack $ ppI i


-- <IRI>
printIRI :: IRI -> Text
printIRI (IRI s) = s


-- <length>
printLength :: Length -> Text
printLength l = pack $ case l of
    Em cc     -> printf "%sem" (ppD cc)
    Ex ll     -> printf "%sex" (ppD ll)
    Px c      -> printf "%spx" (ppD c)
    Inches i  -> printf "%sin" (ppD i)
    Cm c      -> printf "%scm" (ppD c)
    Mm m      -> printf "%smm" (ppD m)
    Pt p      -> printf "%spt" (ppD p)
    Pc p      -> printf "%spc" (ppD p)
    Percent p -> printf "%s%%" (ppD p)
    Length ll -> printf "%s"   (ppD ll)


-- <list-of-Ts>
-- Implementation as class in SerializableContent.hs


-- <name>
printName :: Name -> Text
printName (Name s) = s


-- <number>
printNumber :: Number -> Text
printNumber (Num n) = pack $ ppD n


-- <number-optional-number
printNumberOptionalNumber :: NumberOptionalNumber -> Text
printNumberOptionalNumber n = case n of
    NumOpt a Nothing  -> printNumber a
    NumOpt a (Just b) -> (printNumber a) <> ", " <> (printNumber b)


-- <opacity-value>
printOpacityValue :: OpacityValue -> Text
printOpacityValue (OpacityValue n) = pack $ ppD n


-- <paint>
printPaint :: Paint -> Text
printPaint p = case p of
    PaintNone             -> "none"
    PaintColor c          -> printColor c
    PaintURL url Nothing  -> printURL url
    PaintURL url (Just c) -> printURL url <> (printColor c)
    PaintContextFill      -> "context-fill"
    PaintContextStroke    -> "context-stroke"


-- <percentage>
printPercentage :: Percentage -> Text
printPercentage (Percentage p) = pack $ printf "%s%%" (ppD p)


-- <time>
printTime :: Time -> Text
printTime t = pack $ case t of
    Ms  n -> printf "%sms" (ppD n)
    Sec n -> printf "%ss"  (ppD n)


-- <transform-list>
printTransformFunction :: TransformFunction -> Text
printTransformFunction t = case t of
  Matrix a b c d e f ->
    "matrix(" <> (printNumber a) <> ", "
              <> (printNumber b) <> ", "
              <> (printNumber c) <> ", "
              <> (printNumber d) <> ", "
              <> (printNumber e) <> ", "
              <> (printNumber f) <> ")"
  Translate x (Nothing) ->
    "translate(" <> printNumber x <> ")"
  Translate x (Just y)  ->
    "translate(" <> (printNumber x) <> ", " <> (printNumber y) <> ")"
  Scale x Nothing ->
    "scale(" <> (printNumber x) <> ")"
  Scale x (Just y) ->
    "scale(" <> (printNumber x) <> ", " <> (printNumber y) <> ")"
  Rotate angle Nothing ->
    "rotate(" <> (printNumber angle) <> ")"
  Rotate angle (Just (x, y)) ->
    "rotate(" <> (printNumber angle) <> ", "
              <> (printNumber x) <> ", "
              <> (printNumber y) <> ")"
  SkewX x ->
    "skewX(" <> (printNumber x) <> ")"
  SkewY y ->
    "skewY(" <> (printNumber y) <> ")"
  TransformUnknown -> ""

printTransformList :: TransformList -> Text
printTransformList (TransformList ts) = Data.Text.unwords $ fmap printTransformFunction ts


-- <units>
printUnits :: Units -> Text
printUnits u = case u of
    UserSpaceOnUse    -> "userSpaceOnUse"
    ObjectBoundingBox -> "objectBoundingBox"


-- <URL>
printURL :: URL -> Text
printURL (URL t) = t
