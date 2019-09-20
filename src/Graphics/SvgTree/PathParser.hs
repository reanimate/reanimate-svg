{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.SvgTree.PathParser
  ( transformParser
  , command
  , pathParser
  , viewBoxParser
  , pointData
  , gradientCommand
  , serializePoints
  , serializeCommand
  , serializeGradientCommand
  , serializeCommands
  , serializeViewBox
  ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative        ((*>), (<$), (<$>), (<*), (<*>))
#endif

import           Control.Applicative        ((<|>))
import           Data.Attoparsec.Combinator (option, sepBy, sepBy1)
import           Data.Attoparsec.Text       (Parser, char, digit, many1,
                                             parseOnly, scientific, skipSpace,
                                             string)
import           Data.Scientific            (toRealFloat)

import           Data.List
import qualified Data.Text                  as T
import           Graphics.SvgTree.Misc
import           Graphics.SvgTree.Types
import           Linear                     hiding (angle, point)
import           Text.Printf                (printf)

num :: Parser Double
num = realToFrac <$> (skipSpace *> plusMinus <* skipSpace)
  where doubleNumber :: Parser Double
        doubleNumber = toRealFloat <$> scientific <|> shorthand

        plusMinus = negate <$ string "-" <*> doubleNumber
                 <|> string "+" *> doubleNumber
                 <|> doubleNumber

        shorthand = process' <$> (string "." *> many1 digit)
        process' = either (const 0) id . parseOnly doubleNumber . T.pack . (++) "0."

viewBoxParser :: Parser (Double, Double, Double, Double)
viewBoxParser = (,,,)
       <$> iParse <*> iParse <*> iParse <*> iParse
  where
    iParse = num <* skipSpace

serializeViewBox :: (Double, Double, Double, Double) -> String
serializeViewBox (a, b, c, d) = printf "%s %s %s %s" (ppD a) (ppD b) (ppD c) (ppD d)

commaWsp :: Parser ()
commaWsp = skipSpace *> option () (string "," *> return ()) <* skipSpace

point :: Parser RPoint
point = V2 <$> num <* commaWsp <*> num

pointData :: Parser [RPoint]
pointData = point `sepBy` commaWsp

pathParser :: Parser [PathCommand]
pathParser = skipSpace *> many1 command

command :: Parser PathCommand
command =  (MoveTo OriginAbsolute <$ string "M" <*> pointList)
       <|> (MoveTo OriginRelative <$ string "m" <*> pointList)
       <|> (LineTo OriginAbsolute <$ string "L" <*> pointList)
       <|> (LineTo OriginRelative <$ string "l" <*> pointList)
       <|> (HorizontalTo OriginAbsolute <$ string "H" <*> coordList)
       <|> (HorizontalTo OriginRelative <$ string "h" <*> coordList)
       <|> (VerticalTo OriginAbsolute <$ string "V" <*> coordList)
       <|> (VerticalTo OriginRelative <$ string "v" <*> coordList)
       <|> (CurveTo OriginAbsolute <$ string "C" <*> manyComma curveToArgs)
       <|> (CurveTo OriginRelative <$ string "c" <*> manyComma curveToArgs)
       <|> (SmoothCurveTo OriginAbsolute <$ string "S" <*> pointPairList)
       <|> (SmoothCurveTo OriginRelative <$ string "s" <*> pointPairList)
       <|> (QuadraticBezier OriginAbsolute <$ string "Q" <*> pointPairList)
       <|> (QuadraticBezier OriginRelative <$ string "q" <*> pointPairList)
       <|> (SmoothQuadraticBezierCurveTo OriginAbsolute <$ string "T" <*> pointList)
       <|> (SmoothQuadraticBezierCurveTo OriginRelative <$ string "t" <*> pointList)
       <|> (EllipticalArc OriginAbsolute <$ string "A" <*> manyComma ellipticalArgs)
       <|> (EllipticalArc OriginRelative <$ string "a" <*> manyComma ellipticalArgs)
       <|> (EndPath <$ string "Z" <* commaWsp)
       <|> (EndPath <$ string "z" <* commaWsp)
    where pointList = point `sepBy1` commaWsp
          pointPair = (,) <$> point <* commaWsp <*> point
          pointPairList = pointPair `sepBy1` commaWsp
          coordList = num `sepBy1` commaWsp
          curveToArgs = (,,) <$> (point <* commaWsp)
                             <*> (point <* commaWsp)
                             <*> point
          manyComma a = a `sepBy1` commaWsp

          numComma = num <* commaWsp
          ellipticalArgs = (,,,,,) <$> numComma
                                   <*> numComma
                                   <*> numComma
                                   <*> (fmap (/= 0) numComma)
                                   <*> (fmap (/= 0) numComma)
                                   <*> point

unwordsS :: [ShowS] -> ShowS
unwordsS = foldr (.) id . intersperse (showChar ' ')

serializePoint :: RPoint -> ShowS
serializePoint (V2 x y) = showString (ppD x) . showChar ',' . showString (ppD y)

serializePoints :: [RPoint] -> ShowS
serializePoints = unwordsS . map serializePoint

serializeCoords :: [Coord] -> ShowS
serializeCoords = unwordsS . fmap (showString . ppD)

serializePointPair :: (RPoint, RPoint) -> ShowS
serializePointPair (a, b) = serializePoint a . showChar ' ' . serializePoint b

serializePointPairs :: [(RPoint, RPoint)] -> ShowS
serializePointPairs = unwordsS . fmap serializePointPair

serializePointTriplet :: (RPoint, RPoint, RPoint) -> ShowS
serializePointTriplet (a, b, c) =
    serializePoint a . showChar ' ' . serializePoint b . showChar ' ' . serializePoint c

serializePointTriplets :: [(RPoint, RPoint, RPoint)] -> ShowS
serializePointTriplets = unwordsS . fmap serializePointTriplet

serializeCommands :: [PathCommand] -> ShowS
serializeCommands = unwordsS . fmap serializeCommand

serializeCommand :: PathCommand -> ShowS
serializeCommand p = case p of
  MoveTo OriginAbsolute points -> showChar 'M' . serializePoints points
  MoveTo OriginRelative points -> showChar 'm' . serializePoints points
  LineTo OriginAbsolute points -> showChar 'L' . serializePoints points
  LineTo OriginRelative points -> showChar 'l' . serializePoints points

  HorizontalTo OriginRelative coords -> showChar 'h' . serializeCoords coords
  HorizontalTo OriginAbsolute coords -> showChar 'H' . serializeCoords coords
  VerticalTo OriginAbsolute coords   -> showChar 'V' . serializeCoords coords
  VerticalTo OriginRelative coords   -> showChar 'v' . serializeCoords coords

  CurveTo OriginAbsolute triplets -> showChar 'C' . serializePointTriplets triplets
  CurveTo OriginRelative triplets -> showChar 'c' . serializePointTriplets triplets
  SmoothCurveTo OriginAbsolute pointPairs -> showChar 'S' . serializePointPairs pointPairs
  SmoothCurveTo OriginRelative pointPairs -> showChar 's' . serializePointPairs pointPairs
  QuadraticBezier OriginAbsolute pointPairs -> showChar 'Q' . serializePointPairs pointPairs
  QuadraticBezier OriginRelative pointPairs -> showChar 'q' . serializePointPairs pointPairs
  SmoothQuadraticBezierCurveTo OriginAbsolute points -> showChar 'T' . serializePoints points
  SmoothQuadraticBezierCurveTo OriginRelative points -> showChar 't' . serializePoints points
  EllipticalArc OriginAbsolute args -> showChar 'A' . serializeArgs args
  EllipticalArc OriginRelative args -> showChar 'a' . serializeArgs args
  EndPath -> showChar 'Z'
  where
    serializeArg (a, b, c, d, e, V2 x y) =
        -- ppD a . showChar ' ' .
        -- ppD b . showChar ' ' .
        -- ppD c . showChar ' ' .
        -- shows (fromEnum d) . showChar ' ' .
        -- shows (fromEnum e) . showChar ' ' .
        -- ppD x . showChar ',' .
        -- ppd y
        printf "%s %s %s %d %d %s,%s"
          (ppD a) (ppD b) (ppD c) (fromEnum d) (fromEnum e) (ppD x) (ppD y)
    serializeArgs = unwordsS . fmap serializeArg



transformParser :: Parser Transformation
transformParser = matrixParser
               <|> translationParser
               <|> scaleParser
               <|> rotateParser
               <|> skewYParser
               <|> skewXParser

functionParser :: T.Text -> Parser [Double]
functionParser funcName =
    string funcName *> skipSpace
                    *> char '(' *> skipSpace
                    *> num `sepBy1` commaWsp
                    <* skipSpace <* char ')' <* skipSpace

translationParser :: Parser Transformation
translationParser = do
  args <- functionParser "translate"
  return $ case args of
    [x]    -> Translate x 0
    [x, y] -> Translate x y
    _      -> TransformUnknown

skewXParser :: Parser Transformation
skewXParser = do
  args <- functionParser "skewX"
  return $ case args of
    [x] -> SkewX x
    _   -> TransformUnknown

skewYParser :: Parser Transformation
skewYParser = do
  args <- functionParser "skewY"
  return $ case args of
    [x] -> SkewY x
    _   -> TransformUnknown


scaleParser :: Parser Transformation
scaleParser = do
  args <- functionParser "scale"
  return $ case args of
    [x]    -> Scale x Nothing
    [x, y] -> Scale x (Just y)
    _      -> TransformUnknown

matrixParser :: Parser Transformation
matrixParser = do
  args <- functionParser "matrix"
  return $ case args of
    [a, b, c, d, e, f] ->
        TransformMatrix a b c d e f
    _ -> TransformUnknown

rotateParser :: Parser Transformation
rotateParser = do
  args <- functionParser "rotate"
  return $ case args of
    [angle]       -> Rotate angle Nothing
    [angle, x, y] -> Rotate angle $ Just (x, y)
    _             -> TransformUnknown
{-
rotate(<rotate-angle> [<cx> <cy>]), which specifies a rotation by <rotate-angle> degrees about a given point.

If optional parameters <cx> and <cy> are not supplied, the rotation is about the origin of the current user coordinate system. The operation corresponds to the matrix [cos(a) sin(a) -sin(a) cos(a) 0 0].

If optional parameters <cx> and <cy> are supplied, the rotation is about the point (cx, cy). The operation represents the equivalent of the following specification: translate(<cx>, <cy>) rotate(<rotate-angle>) translate(-<cx>, -<cy>).

skewX(<skew-angle>), which specifies a skew transformation along the x-axis.

skewY(<skew-angle>), which specifies a skew transformation along the y-axis.
    -}
gradientCommand :: Parser GradientPathCommand
gradientCommand =
        (GLine OriginAbsolute <$> (string "L" *> mayPoint))
    <|> (GLine OriginRelative <$> (string "l" *> mayPoint))
    <|> (string "C" *> curveToArgs OriginAbsolute)
    <|> (string "c" *> curveToArgs OriginRelative)
    <|> (GClose <$ string "Z")
  where
    mayPoint = option Nothing $ Just <$> point
    curveToArgs o =
        GCurve o <$> (point <* commaWsp)
                 <*> (point <* commaWsp)
                 <*> mayPoint

serializeGradientCommand :: GradientPathCommand -> ShowS
serializeGradientCommand p = case p of
  GLine OriginAbsolute points -> showChar 'L' . smp points
  GLine OriginRelative points -> showChar 'l' . smp points
  GClose                      -> showChar 'Z'

  GCurve OriginAbsolute a b c -> showChar 'C' . sp a . sp b . smp c
  GCurve OriginRelative a b c -> showChar 'c' . sp a . sp b . smp c
  where
    sp = serializePoint
    smp Nothing   = id
    smp (Just pp) = serializePoint pp
