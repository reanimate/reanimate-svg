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

import           Control.Applicative        ((<|>))
import           Data.Attoparsec.Combinator (option, sepBy, sepBy1)
import           Data.Attoparsec.Text       (Parser, char, digit, many1,
                                             parseOnly, scientific, skipSpace,
                                             string)
import           Data.Functor
import           Data.List
import           Data.Scientific            (toRealFloat)
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

flag :: Parser Bool
flag = fmap (/='0') digit

viewBoxParser :: Parser (Double, Double, Double, Double)
viewBoxParser = (,,,)
       <$> iParse <*> iParse <*> iParse <*> iParse
  where
    iParse = num <* skipSpace

serializeViewBox :: Int -> (Double, Double, Double, Double) -> String
serializeViewBox precision (a, b, c, d) = printf "%s %s %s %s"
  (ppD precision a)
  (ppD precision b)
  (ppD precision c)
  (ppD precision d)

commaWsp :: Parser ()
commaWsp = skipSpace *> option () (string "," $> ()) <* skipSpace

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
          flagComma = flag <* commaWsp
          ellipticalArgs = (,,,,,) <$> numComma
                                   <*> numComma
                                   <*> numComma
                                   <*> flagComma
                                   <*> flagComma
                                   <*> point

unwordsS :: [ShowS] -> ShowS
unwordsS = foldr (.) id . intersperse (showChar ' ')

serializePoint :: Int -> RPoint -> ShowS
serializePoint precision (V2 x y) = showString (ppD precision x) . showChar ',' . showString (ppD precision y)

serializePoints :: Int -> [RPoint] -> ShowS
serializePoints precision = unwordsS . map (serializePoint precision)

serializeCoords :: Int -> [Coord] -> ShowS
serializeCoords precision = unwordsS . fmap (showString . ppD precision)

serializePointPair :: Int -> (RPoint, RPoint) -> ShowS
serializePointPair precision (a, b) = serializePoint precision a . showChar ' ' . serializePoint precision b

serializePointPairs :: Int -> [(RPoint, RPoint)] -> ShowS
serializePointPairs precision = unwordsS . fmap (serializePointPair precision)

serializePointTriplet :: Int -> (RPoint, RPoint, RPoint) -> ShowS
serializePointTriplet precision (a, b, c) =
    serializePoint precision a . showChar ' ' . serializePoint precision b . showChar ' ' . serializePoint precision c

serializePointTriplets :: Int -> [(RPoint, RPoint, RPoint)] -> ShowS
serializePointTriplets precision = unwordsS . fmap (serializePointTriplet precision)

serializeCommands :: Int -> [PathCommand] -> ShowS
serializeCommands precision = unwordsS . fmap (serializeCommand precision)

serializeCommand :: Int -> PathCommand -> ShowS
serializeCommand precision p = case p of
  MoveTo OriginAbsolute points -> showChar 'M' . serializePoints precision points
  MoveTo OriginRelative points -> showChar 'm' . serializePoints precision points
  LineTo OriginAbsolute points -> showChar 'L' . serializePoints precision points
  LineTo OriginRelative points -> showChar 'l' . serializePoints precision points

  HorizontalTo OriginRelative coords -> showChar 'h' . serializeCoords precision coords
  HorizontalTo OriginAbsolute coords -> showChar 'H' . serializeCoords precision coords
  VerticalTo OriginAbsolute coords   -> showChar 'V' . serializeCoords precision coords
  VerticalTo OriginRelative coords   -> showChar 'v' . serializeCoords precision coords

  CurveTo OriginAbsolute triplets -> showChar 'C' . serializePointTriplets precision triplets
  CurveTo OriginRelative triplets -> showChar 'c' . serializePointTriplets precision triplets
  SmoothCurveTo OriginAbsolute pointPairs -> showChar 'S' . serializePointPairs precision pointPairs
  SmoothCurveTo OriginRelative pointPairs -> showChar 's' . serializePointPairs precision pointPairs
  QuadraticBezier OriginAbsolute pointPairs -> showChar 'Q' . serializePointPairs precision pointPairs
  QuadraticBezier OriginRelative pointPairs -> showChar 'q' . serializePointPairs precision pointPairs
  SmoothQuadraticBezierCurveTo OriginAbsolute points -> showChar 'T' . serializePoints precision points
  SmoothQuadraticBezierCurveTo OriginRelative points -> showChar 't' . serializePoints precision points
  EllipticalArc OriginAbsolute args -> showChar 'A' . serializeArgs args
  EllipticalArc OriginRelative args -> showChar 'a' . serializeArgs args
  EndPath -> showChar 'Z'
  where
    serializeArg (a, b, c, d, e, V2 x y) =
        showString $
        printf "%s %s %s %d %d %s,%s"
          (ppD precision a)
          (ppD precision b)
          (ppD precision c)
          (fromEnum d)
          (fromEnum e)
          (ppD precision x)
          (ppD precision y)
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

serializeGradientCommand :: Int -> GradientPathCommand -> ShowS
serializeGradientCommand precision p = case p of
  GLine OriginAbsolute points -> showChar 'L' . smp points
  GLine OriginRelative points -> showChar 'l' . smp points
  GClose                      -> showChar 'Z'

  GCurve OriginAbsolute a b c -> showChar 'C' . sp precision a . sp precision b . smp c
  GCurve OriginRelative a b c -> showChar 'c' . sp precision a . sp precision b . smp c
  where
    sp = serializePoint
    smp Nothing   = id
    smp (Just pp) = serializePoint precision pp
