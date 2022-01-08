{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}
module Graphics.SvgTree.XmlParser
  ( xmlOfDocument
  , unparseDocument
  , unparse
  , xmlOfTree

  , SvgAttributeLens( .. )
  , drawAttributesList
  ) where


import           Text.Read                    (readMaybe)

import           Control.Applicative          (many, (<|>))

import           Codec.Picture                (PixelRGBA8 (..))
import           Control.Lens                 hiding (children, element,
                                               elements, transform)
import           Control.Lens.Unsound
import           Data.Attoparsec.Text         (Parser, parseOnly, string)
import           Data.List                    (foldl', intercalate)
import           Data.Maybe                   (catMaybes, fromMaybe)
import qualified Data.Text                    as T
import           Graphics.SvgTree.ColorParser
import           Graphics.SvgTree.CssParser   (complexNumber, dashArray, num,
                                               numberList, styleString)
import           Graphics.SvgTree.CssTypes    (CssDeclaration (..),
                                               CssElement (..))
import           Graphics.SvgTree.Misc
import           Graphics.SvgTree.PathParser
import           Graphics.SvgTree.Types
import qualified Text.XML.Light               as X
import           Text.XML.Light.Proc          (elChildren, findAttrBy)

import           Text.Printf                  (printf)

{-import Debug.Trace-}

nodeName :: X.Element -> String
nodeName = X.qName . X.elName

setName :: String -> X.Element -> X.Element
setName name elt = elt{ X.elName = X.unqual name }

attributeFinder :: String -> X.Element -> Maybe String
attributeFinder str =
    findAttrBy (\a -> X.qName a == str)

-- | Helper class to help simplify parsing code
-- for various attributes.
class ParseableAttribute a where
  aparse :: String -> Maybe a
  aserialize :: a -> Maybe String

instance ParseableAttribute v => ParseableAttribute (Maybe v) where
  aparse = fmap Just . aparse
  aserialize = (>>= aserialize)

instance ParseableAttribute String where
  aparse = Just
  aserialize = Just

instance ParseableAttribute Number where
  aparse = parseMayStartDot complexNumber
  aserialize = Just . serializeNumber

instance ParseableAttribute [Number] where
  aparse = parse dashArray
  aserialize = Just . serializeDashArray

instance ParseableAttribute [Double] where
  aparse = parse numberList
  aserialize = Just . serializeDashArray . map Num

instance ParseableAttribute PixelRGBA8 where
  aparse = parse colorParser
  aserialize = Just . colorSerializer

instance ParseableAttribute [PathCommand] where
  aparse = parse pathParser
  aserialize v = Just $ serializeCommands v ""

instance ParseableAttribute GradientPathCommand where
  aparse = parse gradientCommand
  aserialize v = Just $ serializeGradientCommand v ""

instance ParseableAttribute [RPoint] where
  aparse = parse pointData
  aserialize v = Just $ serializePoints v ""

instance ParseableAttribute Double where
  aparse = parseMayStartDot num
  aserialize v = Just $ printf "%s" (ppD v)

instance ParseableAttribute Int where
  aparse = fmap (round :: Double -> Int) . aparse
  aserialize v = Just $ printf "%d" v

instance ParseableAttribute Texture where
  aparse = parse textureParser
  aserialize = Just . textureSerializer

instance ParseableAttribute [Transformation] where
  aparse = parse $ many transformParser
  aserialize = Just . serializeTransformations

instance ParseableAttribute Alignment where
  aparse s = Just $ case s of
    "none"     -> AlignNone
    "xMinYMin" -> AlignxMinYMin
    "xMidYMin" -> AlignxMidYMin
    "xMaxYMin" -> AlignxMaxYMin
    "xMinYMid" -> AlignxMinYMid
    "xMidYMid" -> AlignxMidYMid
    "xMaxYMid" -> AlignxMaxYMid
    "xMinYMax" -> AlignxMinYMax
    "xMidYMax" -> AlignxMidYMax
    "xMaxYMax" -> AlignxMaxYMax
    _          -> _aspectRatioAlign defaultSvg

  aserialize v = Just $ case v of
    AlignNone     -> "none"
    AlignxMinYMin -> "xMinYMin"
    AlignxMidYMin -> "xMidYMin"
    AlignxMaxYMin -> "xMaxYMin"
    AlignxMinYMid -> "xMinYMid"
    AlignxMidYMid -> "xMidYMid"
    AlignxMaxYMid -> "xMaxYMid"
    AlignxMinYMax -> "xMinYMax"
    AlignxMidYMax -> "xMidYMax"
    AlignxMaxYMax -> "xMaxYMax"

instance ParseableAttribute MeshGradientType where
  aparse s = Just $ case s of
    "bilinear" -> GradientBilinear
    "bicubic"  -> GradientBicubic
    _          -> GradientBilinear

  aserialize v = Just $ case v of
    GradientBilinear -> "bilinear"
    GradientBicubic  -> "bicubic"

instance ParseableAttribute MeetSlice where
  aparse s = case s of
    "meet"  -> Just Meet
    "slice" -> Just Slice
    _       -> Nothing

  aserialize v = Just $ case v of
    Meet  -> "meet"
    Slice -> "slice"

instance ParseableAttribute PreserveAspectRatio where
  aserialize v = Just $ defer <> align <> meetSlice where
    defer = if _aspectRatioDefer v then "defer " else ""
    align = fromMaybe "" . aserialize $ _aspectRatioAlign v
    meetSlice = fromMaybe "" $ aserialize =<< _aspectRatioMeetSlice v

  aparse s = case words s of
      [] -> Nothing
      [align] -> Just $ defaultSvg { _aspectRatioAlign = alignOf align }
      ["defer", align] ->
          Just $ defaultSvg
            { _aspectRatioDefer = True
            , _aspectRatioAlign = alignOf align
            }
      [align, meet] ->
          Just $ defaultSvg
            { _aspectRatioMeetSlice = aparse meet
            , _aspectRatioAlign = alignOf align
            }
      ["defer", align, meet] ->
          Just $ PreserveAspectRatio
              { _aspectRatioDefer = True
              , _aspectRatioAlign = alignOf align
              , _aspectRatioMeetSlice = aparse meet
              }
      _ -> Nothing
    where
      alignOf = fromMaybe (_aspectRatioAlign defaultSvg) . aparse

instance ParseableAttribute Cap where
  aparse s = case s of
    "butt"   -> Just CapButt
    "round"  -> Just CapRound
    "square" -> Just CapSquare
    _        -> Nothing

  aserialize c = Just $ case c of
    CapButt   -> "butt"
    CapRound  -> "round"
    CapSquare -> "square"

instance ParseableAttribute TextAnchor where
  aparse s = case s of
    "middle" -> Just TextAnchorMiddle
    "start"  -> Just TextAnchorStart
    "end"    -> Just TextAnchorEnd
    _        -> Nothing

  aserialize t = Just $ case t of
    TextAnchorMiddle -> "middle"
    TextAnchorStart  -> "start"
    TextAnchorEnd    -> "end"

instance ParseableAttribute ElementRef where
  aparse s = case parseOnly pa $ T.pack s of
     Left _  -> Nothing
     Right v -> Just v
    where
      pa = (RefNone <$ string "none")
        <|> (Ref <$> urlRef)

  aserialize c = Just $ case c of
    Ref r   -> "url(#" <> r <> ")"
    RefNone -> "none"

instance ParseableAttribute LineJoin where
  aparse s = case s of
    "miter" -> Just JoinMiter
    "round" -> Just JoinRound
    "bevel" -> Just JoinBevel
    _       -> Nothing

  aserialize j = Just $ case j of
    JoinMiter -> "miter"
    JoinRound -> "round"
    JoinBevel -> "bevel"

instance ParseableAttribute CoordinateUnits where
  aparse s = case s of
    "userSpaceOnUse"    -> Just CoordUserSpace
    "objectBoundingBox" -> Just CoordBoundingBox
    _                   -> Just CoordBoundingBox

  aserialize uni = Just $ case uni of
    CoordUserSpace   -> "userSpaceOnUse"
    CoordBoundingBox -> "objectBoundingBox"

instance ParseableAttribute Spread where
  aparse s = case s of
    "pad"     -> Just SpreadPad
    "reflect" -> Just SpreadReflect
    "repeat"  -> Just SpreadRepeat
    _         -> Nothing

  aserialize s = Just $ case s of
    SpreadPad     -> "pad"
    SpreadReflect -> "reflect"
    SpreadRepeat  -> "repeat"

instance ParseableAttribute FillRule where
  aparse s = case s of
    "nonzero" -> Just FillNonZero
    "evenodd" -> Just FillEvenOdd
    _         -> Nothing

  aserialize f = Just $ case f of
    FillNonZero -> "nonzero"
    FillEvenOdd -> "evenodd"

instance ParseableAttribute TextAdjust where
  aparse s = Just $ case s of
    "spacing"          -> TextAdjustSpacing
    "spacingAndGlyphs" -> TextAdjustSpacingAndGlyphs
    _                  -> TextAdjustSpacing

  aserialize a = Just $ case a of
    TextAdjustSpacing          -> "spacing"
    TextAdjustSpacingAndGlyphs -> "spacingAndGlyphs"

instance ParseableAttribute MarkerUnit where
  aparse s = case s of
    "strokeWidth"    -> Just MarkerUnitStrokeWidth
    "userSpaceOnUse" -> Just MarkerUnitUserSpaceOnUse
    _                -> Nothing

  aserialize u = Just $ case u of
    MarkerUnitStrokeWidth    -> "strokeWidth"
    MarkerUnitUserSpaceOnUse -> "userSpaceOnUse"

instance ParseableAttribute Overflow where
  aparse s = case s of
    "visible" -> Just OverflowVisible
    "hidden"  -> Just OverflowHidden
    _         -> Nothing

  aserialize u = Just $ case u of
    OverflowVisible -> "visible"
    OverflowHidden  -> "hidden"

instance ParseableAttribute MarkerOrientation where
  aparse s = case (s, readMaybe s) of
    ("auto", _) -> Just OrientationAuto
    (_, Just f) -> Just $ OrientationAngle f
    _           -> Nothing

  aserialize s = Just $ case s of
    OrientationAuto    -> "auto"
    OrientationAngle f -> show f

instance ParseableAttribute (Double, Double, Double, Double) where
  aparse = parse viewBoxParser
  aserialize = Just . serializeViewBox

instance ParseableAttribute TextPathMethod where
  aparse s = case s of
    "align"   -> Just TextPathAlign
    "stretch" -> Just TextPathStretch
    _         -> Nothing
  aserialize m = Just $ case m of
    TextPathAlign   -> "align"
    TextPathStretch -> "stretch"

instance ParseableAttribute TextPathSpacing where
  aparse s = case s of
    "auto"  -> Just TextPathSpacingAuto
    "exact" -> Just TextPathSpacingExact
    _       -> Nothing

  aserialize s = Just $ case s of
    TextPathSpacingAuto  -> "auto"
    TextPathSpacingExact -> "exact"

instance ParseableAttribute CompositeOperator where
  aparse s = case s of
    "over"       -> Just CompositeOver
    "in"         -> Just CompositeIn
    "out"        -> Just CompositeOut
    "atop"       -> Just CompositeAtop
    "xor"        -> Just CompositeXor
    "arithmetic" -> Just CompositeArithmetic
    _            -> Nothing

  aserialize v = Just $ case v of
    CompositeOver       -> "over"
    CompositeIn         -> "in"
    CompositeOut        -> "out"
    CompositeAtop       -> "atop"
    CompositeXor        -> "xor"
    CompositeArithmetic -> "arithmetic"

instance ParseableAttribute FilterSource where
  aparse s = Just $ case s of
    "SourceGraphic"   -> SourceGraphic
    "SourceAlpha"     -> SourceAlpha
    "BackgroundImage" -> BackgroundImage
    "BackgroundAlpha" -> BackgroundAlpha
    "FillPaint"       -> FillPaint
    "StrokePaint"     -> StrokePaint
    _                 -> SourceRef s

  aserialize v = Just $ case v of
    SourceGraphic   -> "SourceGraphic"
    SourceAlpha     -> "SourceAlpha"
    BackgroundImage -> "BackgroundImage"
    BackgroundAlpha -> "BackgroundAlpha"
    FillPaint       -> "FillPaint"
    StrokePaint     -> "StrokePaint"
    SourceRef s     -> s

instance ParseableAttribute FuncType where
  aparse s = case s of
    "identity" -> Just FIdentity
    "table"    -> Just FTable
    "discrete" -> Just FDiscrete
    "linear"   -> Just FLinear
    "gamma"    -> Just FGamma
    _          -> Nothing

  aserialize v = Just $ case v of
    FIdentity -> "identity"
    FTable    -> "table"
    FDiscrete -> "discrete"
    FLinear   -> "linear"
    FGamma    -> "gamma"

instance ParseableAttribute BlendMode where
  aparse s = case s of
    "normal"      -> Just Normal
    "multiply"    -> Just Multiply
    "screen"      -> Just Screen
    "overlay"     -> Just Overlay
    "darken"      -> Just Darken
    "lighten"     -> Just Lighten
    "color-dodge" -> Just ColorDodge
    "color-burn"  -> Just ColorBurn
    "hard-light"  -> Just HardLight
    "soft-light"  -> Just SoftLight
    "difference"  -> Just Difference
    "exclusion"   -> Just Exclusion
    "hue"         -> Just Hue
    "saturation"  -> Just Saturation
    "color"       -> Just Color
    "luminosity"  -> Just Luminosity
    _             -> Nothing

  aserialize v = Just $ case v of
    Normal     -> "normal"
    Multiply   -> "multiply"
    Screen     -> "screen"
    Overlay    -> "overlay"
    Darken     -> "darken"
    Lighten    -> "lighten"
    ColorDodge -> "color-dodge"
    ColorBurn  -> "color-burn"
    HardLight  -> "hard-light"
    SoftLight  -> "soft-light"
    Difference -> "difference"
    Exclusion  -> "exclusion"
    Hue        -> "hue"
    Saturation -> "saturation"
    Color      -> "color"
    Luminosity -> "luminosity"


instance ParseableAttribute ColorMatrixType where
  aparse s = case s of
    "matrix"           -> Just Matrix
    "saturate"         -> Just Saturate
    "hueRotate"        -> Just HueRotate
    "luminanceToAlpha" -> Just LuminanceToAlpha
    _                  -> Nothing

  aserialize v = Just $ case v of
    Matrix           -> "matrix"
    Saturate         -> "saturate"
    HueRotate        -> "hueRotate"
    LuminanceToAlpha -> "luminanceToAlpha"

instance ParseableAttribute StitchTiles where
  aparse s = case s of
    "noStitch" -> Just NoStitch
    "stitch"   -> Just Stitch
    _          -> Nothing

  aserialize v = Just $ case v of
    NoStitch -> "noStitch"
    Stitch   -> "stitch"

instance ParseableAttribute TurbulenceType where
  aparse s = case s of
    "fractalNoise" -> Just FractalNoiseType
    "turbulence"   -> Just TurbulenceType
    _              -> Nothing

  aserialize v = Just $ case v of
    FractalNoiseType -> "fractalNoise"
    TurbulenceType   -> "turbulence"

instance ParseableAttribute ChannelSelector where
  aparse s = case s of
    "R" -> Just ChannelR
    "G" -> Just ChannelG
    "B" -> Just ChannelB
    "A" -> Just ChannelA
    _   -> Nothing

  aserialize v = Just $ case v of
    ChannelR -> "R"
    ChannelG -> "G"
    ChannelB -> "B"
    ChannelA -> "A"

instance ParseableAttribute OperatorType where
  aparse s = case s of
    "over"       -> Just OperatorOver
    "in"         -> Just OperatorIn
    "out"        -> Just OperatorOut
    "atop"       -> Just OperatorAtop
    "xor"        -> Just OperatorXor
    "lighter"    -> Just OperatorLighter
    "arithmetic" -> Just OperatorArithmetic
    _            -> Nothing

  aserialize v = Just $ case v of
    OperatorOver       -> "over"
    OperatorIn         -> "in"
    OperatorOut        -> "out"
    OperatorAtop       -> "atop"
    OperatorXor        -> "xor"
    OperatorLighter    -> "lighter"
    OperatorArithmetic -> "arithmetic"

instance ParseableAttribute NumberOptionalNumber where
  aparse s = case s of
    _  -> Nothing                                        -- TODO

  aserialize v = Just $ case v of
    Num1 a   -> show a
    Num2 a b -> show a ++ ", " ++ show b

instance ParseableAttribute Bool where
  aparse s = case s of
    "false" -> Just False
    "true"  -> Just True
    _       -> Nothing

  aserialize v = Just $ case v of
    False -> "false"
    True  -> "true"

instance ParseableAttribute EdgeMode where
  aparse s = case s of
    "duplicate" -> Just EdgeDuplicate
    "wrap"      -> Just EdgeWrap
    "none"      -> Just EdgeNone
    _           -> Nothing

  aserialize v = Just $ case v of
    EdgeDuplicate -> "duplicate"
    EdgeWrap      -> "wrap"
    EdgeNone      -> "none"

instance ParseableAttribute (Number, Maybe Number) where
  aparse s = case aparse s of
    Just [x]   -> Just (x, Nothing)
    Just [x,y] -> Just (x, Just y)
    _          -> Nothing

  aserialize (x, Nothing)  = aserialize [x]
  aserialize (x, Just y) = aserialize [x, y]

instance ParseableAttribute (Double, Maybe Double) where
  aparse s = case aparse s of
    Just [x]   -> Just (x, Nothing)
    Just [x,y] -> Just (x, Just y)
    _          -> Nothing

  aserialize (x, Nothing)  = aserialize [x]
  aserialize (x, Just y) = aserialize [x, y]

parse :: Parser a -> String -> Maybe a
parse p str = case parseOnly p (T.pack str) of
  Left _  -> Nothing
  Right r -> Just r

parseMayStartDot :: Parser a -> String -> Maybe a
parseMayStartDot p l@('.':_) = parse p ('0':l)
parseMayStartDot p l         = parse p l

xmlUpdate :: (XMLUpdatable a) => a -> X.Element -> a
xmlUpdate initial el = foldl' grab initial attributes
  where
    grab value updater =
        case attributeFinder (_attributeName updater) el of
          Nothing -> value
          Just v  -> _attributeUpdater updater value v

xmlUnparse :: (WithDefaultSvg a, XMLUpdatable a) => X.Element -> a
xmlUnparse = xmlUpdate defaultSvg

xmlUnparseWithDrawAttr
    :: (WithDefaultSvg a, XMLUpdatable a, HasDrawAttributes a)
    => X.Element -> a
xmlUnparseWithDrawAttr e =
    xmlUnparse e & drawAttributes .~ xmlUnparse e

data SvgAttributeLens t = SvgAttributeLens
  { _attributeName       :: String
  , _attributeUpdater    :: t -> String -> t
  , _attributeSerializer :: t -> Maybe String
  }

class XMLUpdatable treeNode where
  xmlTagName :: treeNode -> String
  attributes :: [SvgAttributeLens treeNode]

  serializeTreeNode :: treeNode -> Maybe X.Element

setChildren :: X.Element -> [X.Content] -> X.Element
setChildren xNode children = xNode { X.elContent = children }

updateWithAccessor :: XMLUpdatable b => (a -> [b]) -> a -> Maybe X.Element -> Maybe X.Element
updateWithAccessor        _    _ Nothing = Nothing
updateWithAccessor accessor node (Just xNode) =
    Just . setChildren xNode . fmap  X.Elem . catMaybes $ serializeTreeNode <$> accessor node

genericSerializeNode :: (XMLUpdatable treeNode) => treeNode -> Maybe X.Element
genericSerializeNode node =
    Just . X.unode (xmlTagName node) $ concatMap generateAttribute attributes
  where
    generateAttribute attr = case _attributeSerializer attr node of
      Nothing -> []
      Just str -> return X.Attr
        { X.attrKey = xName $ _attributeName attr
        , X.attrVal = str
        }
        where
         xName "href" =
            X.QName { X.qName = "href"
                    , X.qURI = Nothing
                    , X.qPrefix = Just "xlink" }
         xName h = X.unqual h


mergeAttributes :: X.Element -> X.Element -> X.Element
mergeAttributes thisXml otherXml =
    thisXml { X.elAttribs = X.elAttribs otherXml ++ X.elAttribs thisXml }

genericSerializeWithDrawAttr :: (XMLUpdatable treeNode, HasDrawAttributes treeNode)
                             => treeNode -> Maybe X.Element
genericSerializeWithDrawAttr node = mergeAttributes <$> thisXml <*> drawAttrNode where
  thisXml = genericSerializeNode node
  drawAttrNode = genericSerializeNode $ node ^. drawAttributes

type CssUpdater a =
    a -> [[CssElement]] -> a

opacitySetter :: String -> Lens' a (Maybe Float) -> SvgAttributeLens a
opacitySetter attribute elLens =
    SvgAttributeLens attribute updater serializer
  where
    serializer a = printf "%s" . ppF <$> a ^. elLens
    updater el str = case parseMayStartDot num str of
        Nothing -> el
        Just v  -> el & elLens ?~ realToFrac v

type Serializer e = e -> Maybe String

parserSetter :: String -> Lens' a e -> (String -> Maybe e) -> Serializer e
             -> SvgAttributeLens a
parserSetter attribute elLens parser serialize =
    SvgAttributeLens attribute updater serializer
  where
    updater el str = case parser str of
        Nothing -> el
        Just v  -> el & elLens .~ v

    serializer  a = serialize $ a ^. elLens

parseIn :: (Eq a, WithDefaultSvg s, ParseableAttribute a)
        => String -> Lens' s a -> SvgAttributeLens s
parseIn attribute elLens =
    SvgAttributeLens attribute updater serializer
  where
    updater el str = case aparse str of
        Nothing -> el
        Just v  -> el & elLens .~ v

    serializer a
      | v /= defaultVal = aserialize v
      | otherwise = Nothing
      where
        v = a ^. elLens
        defaultVal = defaultSvg ^. elLens

parserMaybeSetter :: String -> Lens' a (Maybe e) -> (String -> Maybe e) -> Serializer e
                 -> SvgAttributeLens a
parserMaybeSetter attribute elLens parser serialize =
    SvgAttributeLens attribute updater serializer
  where
    updater el str = case parser str of
        Nothing -> el
        Just v  -> el & elLens ?~ v

    serializer a = (a ^. elLens) >>= serialize

classSetter :: SvgAttributeLens DrawAttributes
classSetter = SvgAttributeLens "class" updater serializer
  where
    updater el str =
      el & attrClass .~ T.split (== ' ') (T.pack str)

    serializer a = case a ^. attrClass of
      []  -> Nothing
      lst -> Just . T.unpack $ T.intercalate " " lst

cssUniqueNumber :: ASetter el el
                   a (Maybe Number)
                -> CssUpdater el
cssUniqueNumber setter attr ((CssNumber n:_):_) =
    attr & setter ?~ n
cssUniqueNumber _ attr _ = attr

cssUniqueFloat :: (Fractional n)
               => ASetter el el a (Maybe n)
               -> CssUpdater el
cssUniqueFloat setter attr ((CssNumber (Num n):_):_) =
    attr & setter ?~ realToFrac n
cssUniqueFloat _ attr _ = attr

cssUniqueMayFloat :: ASetter el el a (Maybe Double)
               -> CssUpdater el
cssUniqueMayFloat setter attr ((CssNumber (Num n):_):_) =
    attr & setter ?~ n
cssUniqueMayFloat _ attr _ = attr

cssIdentAttr :: ParseableAttribute a => Lens' el a -> CssUpdater el
cssIdentAttr setter attr ((CssIdent i:_):_) = case aparse $ T.unpack i of
    Nothing -> attr
    Just v  -> attr & setter .~ v
cssIdentAttr _ attr _ = attr

fontFamilyParser :: CssUpdater DrawAttributes
fontFamilyParser attr (lst:_) = attr & fontFamily .~ fontNames
  where
    fontNames = Just $ T.unpack <$> extractString lst

    extractString []                 = []
    extractString (CssIdent n:rest)  = n : extractString rest
    extractString (CssString n:rest) = n : extractString rest
    extractString (_:rest)           = extractString rest
fontFamilyParser attr _ = attr


cssUniqueTexture :: ASetter el el
                    a (Maybe Texture)
                 -> CssUpdater el
cssUniqueTexture setter attr css = case css of
  ((CssIdent "none":_):_) -> attr & setter ?~ FillNone
  ((CssColor c:_):_) -> attr & setter ?~ ColorRef c
  ((CssFunction "url" [CssReference c]:_):_) ->
        attr & setter ?~ TextureRef (T.unpack c)
  _ -> attr

cssUniqueColor :: ASetter el el a PixelRGBA8 -> CssUpdater el
cssUniqueColor setter attr css = case css of
  ((CssColor c:_):_) -> attr & setter .~ c
  _                  -> attr

cssElementRefSetter :: Lens' el (Maybe ElementRef) -> CssUpdater el
cssElementRefSetter setter attr ((CssFunction "url" [CssReference c]:_):_) =
    attr & setter ?~ Ref (T.unpack c)
cssElementRefSetter setter attr ((CssIdent "none":_):_) =
    attr & setter ?~ RefNone
cssElementRefSetter _ attr _ = attr

cssMayStringSetter :: ASetter el el a (Maybe String) -> CssUpdater el
cssMayStringSetter setter attr ((CssIdent i:_):_) =
    attr & setter ?~ T.unpack i
cssMayStringSetter setter attr ((CssString i:_):_) =
    attr & setter ?~ T.unpack i
cssMayStringSetter _ attr _ = attr

cssNullSetter :: CssUpdater a
cssNullSetter attr _ = attr

cssDashArray :: ASetter el el a (Maybe [Number]) -> CssUpdater el
cssDashArray setter attr (lst:_) =
  case [n | CssNumber n <- lst ] of
    [] -> attr
    v  -> attr & setter ?~ v
cssDashArray _ attr _ = attr


drawAttributesList :: [(SvgAttributeLens DrawAttributes, CssUpdater DrawAttributes)]
drawAttributesList =
  [("stroke-width" `parseIn` strokeWidth, cssUniqueNumber strokeWidth)
  ,("stroke" `parseIn` strokeColor, cssUniqueTexture strokeColor)
  ,("fill" `parseIn` fillColor, cssUniqueTexture fillColor)
  ,("stroke-linecap" `parseIn` strokeLineCap, cssIdentAttr strokeLineCap)
  ,("stroke-linejoin" `parseIn` strokeLineJoin, cssIdentAttr strokeLineJoin)
  ,("stroke-miterlimit" `parseIn` strokeMiterLimit,
       cssUniqueMayFloat strokeMiterLimit)

  ,("transform" `parseIn` transform, const)
  ,(opacitySetter "opacity" groupOpacity, cssUniqueFloat groupOpacity)
  ,(opacitySetter "fill-opacity" fillOpacity, cssUniqueFloat fillOpacity)
  ,(opacitySetter "stroke-opacity" strokeOpacity, cssUniqueFloat strokeOpacity)
  ,("font-size" `parseIn` fontSize, cssUniqueNumber fontSize)
  ,(parserMaybeSetter "font-family" fontFamily (Just . commaSeparate)
      (Just . intercalate ", "), fontFamilyParser)

  ,("fill-rule" `parseIn` fillRule, cssIdentAttr fillRule)
  ,("clip-rule" `parseIn` clipRule, cssIdentAttr clipRule)
  ,("mask" `parseIn` maskRef, cssElementRefSetter maskRef)
  ,(classSetter, cssNullSetter) -- can't set class in CSS
  ,("id" `parseIn` attrId, cssMayStringSetter attrId)
  ,("stroke-dashoffset" `parseIn` strokeOffset,
      cssUniqueNumber strokeOffset)
  ,("stroke-dasharray" `parseIn` strokeDashArray, cssDashArray strokeDashArray)
  ,("text-anchor" `parseIn` textAnchor, cssIdentAttr textAnchor)
  ,("clip-path" `parseIn` clipPathRef, cssElementRefSetter clipPathRef)
  ,("marker-end" `parseIn` markerEnd, cssElementRefSetter markerEnd)
  ,("marker-start" `parseIn` markerStart, cssElementRefSetter markerStart)
  ,("marker-mid" `parseIn` markerMid, cssElementRefSetter markerMid)
  ,("filter" `parseIn` filterRef, cssNullSetter)
  ]
  where
    commaSeparate =
        fmap (T.unpack . T.strip) . T.split (',' ==) . T.pack

serializeDashArray :: [Number] -> String
serializeDashArray =
   intercalate ", " . fmap serializeNumber

instance XMLUpdatable DrawAttributes where
  xmlTagName _ = "DRAWATTRIBUTES"
  attributes =
      styleAttribute drawAttributesList : fmap fst drawAttributesList
  serializeTreeNode = genericSerializeNode

styleAttribute :: [(SvgAttributeLens a, CssUpdater a)] -> SvgAttributeLens a
styleAttribute styleAttrs = SvgAttributeLens
  { _attributeName       = "style"
  , _attributeUpdater    = updater
  , _attributeSerializer = const Nothing
  }
  where
    updater attrs style = case parse styleString style of
        Nothing    -> attrs
        Just decls -> foldl' applyer attrs decls

    cssUpdaters = [(T.pack $ _attributeName n, u) | (n, u) <- styleAttrs]
    applyer value (CssDeclaration txt elems) =
        case lookup txt cssUpdaters of
          Nothing -> value
          Just f  -> f value elems

instance XMLUpdatable Rectangle where
  xmlTagName _ = "rect"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    ["width" `parseIn` rectWidth
    ,"height" `parseIn` rectHeight
    ,"x" `parseIn` (rectUpperLeftCorner._1)
    ,"y" `parseIn` (rectUpperLeftCorner._2)
    ,"rx" `parseIn` (rectCornerRadius._1)
    ,"ry" `parseIn` (rectCornerRadius._2)
    ]

instance XMLUpdatable Image where
  xmlTagName _ = "image"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    ["width" `parseIn` imageWidth
    ,"height" `parseIn` imageHeight
    ,"x" `parseIn` (imageCornerUpperLeft._1)
    ,"y" `parseIn` (imageCornerUpperLeft._2)
    ,parserSetter "href" imageHref (Just . dropSharp) Just
    ,"preserveAspectRatio" `parseIn` imageAspectRatio
    ]

instance XMLUpdatable Line where
  xmlTagName _ = "line"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    ["x1" `parseIn` (linePoint1._1)
    ,"y1" `parseIn` (linePoint1._2)
    ,"x2" `parseIn` (linePoint2._1)
    ,"y2" `parseIn` (linePoint2._2)
    ]

instance XMLUpdatable Ellipse where
  xmlTagName _ = "ellipse"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    ["cx" `parseIn` (ellipseCenter._1)
    ,"cy" `parseIn` (ellipseCenter._2)
    ,"rx" `parseIn` ellipseXRadius
    ,"ry" `parseIn` ellipseYRadius
    ]

instance XMLUpdatable Circle where
  xmlTagName _ = "circle"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    ["cx" `parseIn` (circleCenter._1)
    ,"cy" `parseIn` (circleCenter._2)
    ,"r" `parseIn` circleRadius
    ]

instance XMLUpdatable Mask where
  xmlTagName _ = "mask"
  serializeTreeNode node =
      updateWithAccessor _maskContent node $
          genericSerializeWithDrawAttr node

  attributes =
    ["x" `parseIn` (maskPosition._1)
    ,"y" `parseIn` (maskPosition._2)
    ,"width" `parseIn` maskWidth
    ,"height" `parseIn` maskHeight
    ,"maskContentUnits" `parseIn` maskContentUnits
    ,"maskUnits" `parseIn` maskUnits
    ]

instance XMLUpdatable ClipPath where
  xmlTagName _ = "clipPath"
  serializeTreeNode node =
      updateWithAccessor _clipPathContent node $
          genericSerializeWithDrawAttr node
  attributes =
    ["clipPathUnits" `parseIn` clipPathUnits]

instance XMLUpdatable Polygon where
  xmlTagName _ = "polygon"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes = ["points" `parseIn` polygonPoints]

instance XMLUpdatable PolyLine where
  xmlTagName _ =  "polyline"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes = ["points" `parseIn` polyLinePoints]

instance XMLUpdatable Path where
  xmlTagName _ =  "path"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes = ["d" `parseIn` pathDefinition]

instance XMLUpdatable MeshGradientPatch where
  xmlTagName _ = "meshpatch"
  attributes = []
  serializeTreeNode node =
     updateWithAccessor _meshGradientPatchStops node $ genericSerializeNode node

instance XMLUpdatable MeshGradientRow where
  xmlTagName _ = "meshrow"
  serializeTreeNode node =
     updateWithAccessor _meshGradientRowPatches node $ genericSerializeNode node
  attributes = []

instance XMLUpdatable MeshGradient where
  xmlTagName _ = "meshgradient"
  serializeTreeNode node =
     updateWithAccessor _meshGradientRows node $ genericSerializeWithDrawAttr node
  attributes =
    ["x" `parseIn` meshGradientX
    ,"y" `parseIn` meshGradientY
    ,"type" `parseIn` meshGradientType
    ,"gradientUnits" `parseIn` meshGradientUnits
    ,"gradientTransform" `parseIn` meshGradientTransform
    ]


instance XMLUpdatable LinearGradient where
  xmlTagName _ = "linearGradient"
  serializeTreeNode node =
     updateWithAccessor _linearGradientStops node $ genericSerializeWithDrawAttr node

  attributes =
    ["gradientTransform" `parseIn` linearGradientTransform
    ,"gradientUnits" `parseIn` linearGradientUnits
    ,"spreadMethod" `parseIn` linearGradientSpread
    ,"x1" `parseIn` (linearGradientStart._1)
    ,"y1" `parseIn` (linearGradientStart._2)
    ,"x2" `parseIn` (linearGradientStop._1)
    ,"y2" `parseIn` (linearGradientStop._2)
    ]

instance XMLUpdatable Tree where
  xmlTagName _ = "TREE"
  attributes = []
  serializeTreeNode e = case e ^. treeBranch of
    NoNode -> Nothing
    UseNode u _ -> serializeTreeNode u
    GroupNode g -> serializeTreeNode g
    SymbolNode s -> setName "symbol" <$> serializeTreeNode s
    DefinitionNode d -> setName "defs" <$> serializeTreeNode d
    FilterNode g -> serializeTreeNode g
    PathNode p -> serializeTreeNode p
    CircleNode c -> serializeTreeNode c
    PolyLineNode p -> serializeTreeNode p
    PolygonNode p -> serializeTreeNode p
    EllipseNode el -> serializeTreeNode el
    LineNode l -> serializeTreeNode l
    RectangleNode r -> serializeTreeNode r
    TextNode Nothing t -> serializeTreeNode t
    ImageNode i -> serializeTreeNode i
    LinearGradientNode l -> serializeTreeNode l
    RadialGradientNode r -> serializeTreeNode r
    MeshGradientNode m -> serializeTreeNode m
    PatternNode p -> serializeTreeNode p
    MarkerNode m -> serializeTreeNode m
    MaskNode m -> serializeTreeNode m
    ClipPathNode c -> serializeTreeNode c
    TextNode (Just p) t -> do
       textNode <- serializeTreeNode t
       pathNode <- serializeTreeNode p
       let sub = [X.Elem . setChildren pathNode $ X.elContent textNode]
       return $ setChildren textNode sub
    SvgNode doc -> Just $ xmlOfDocument doc


isNotNone :: Tree -> Bool
isNotNone None = False
isNotNone _ = True

instance XMLUpdatable Group where
  xmlTagName _ = "g"
  serializeTreeNode node =
     updateWithAccessor (filter isNotNone . _groupChildren) node $
        genericSerializeWithDrawAttr node
  attributes =
     ["viewBox" `parseIn` groupViewBox
     ,"preserveAspectRatio" `parseIn` groupAspectRatio
     ]

instance XMLUpdatable Filter where
  xmlTagName _ = "filter"
  serializeTreeNode node =
     updateWithAccessor _filterChildren node $
        genericSerializeWithDrawAttr node
  attributes =
    [ "width" `parseIn` filterWidth
    , "height" `parseIn` filterHeight
    , "x" `parseIn` filterX
    , "y" `parseIn` filterY ]

instance XMLUpdatable FilterElement where
  xmlTagName _ = "FilterElement"
  serializeTreeNode fe = flip mergeAttributes <$> genericSerializeNode fe <*>
    case fe of
      FEBlend b             -> serializeTreeNode b
      FEColorMatrix m       -> serializeTreeNode m
      FEComposite c         -> serializeTreeNode c
      FEGaussianBlur b      -> serializeTreeNode b
      FETurbulence t        -> serializeTreeNode t
      FEDisplacementMap d   -> serializeTreeNode d
      FETile t              -> serializeTreeNode t
      FEFlood f             -> serializeTreeNode f
      FEOffset o            -> serializeTreeNode o
      FEMerge m             -> serializeTreeNode m
      FEMergeNode n         -> serializeTreeNode n
      FEImage i             -> serializeTreeNode i
      FEComponentTransfer f -> serializeTreeNode f
      FEFuncA f             -> serializeTreeNode f
      FEFuncR f             -> serializeTreeNode f
      FEFuncG f             -> serializeTreeNode f
      FEFuncB f             -> serializeTreeNode f
      FESpecularLighting s  -> serializeTreeNode s
      FEConvolveMatrix c    -> serializeTreeNode c
      FEDiffuseLighting d   -> serializeTreeNode d
      FEMorphology m        -> serializeTreeNode m
      FEDropShadow d        -> serializeTreeNode d
      _                     -> error $
        "Unsupported element: " ++ show fe ++ ". Please submit bug on github."
  attributes =
    [ "result" `parseIn` (filterAttributes . filterResult)]

instance XMLUpdatable ConvolveMatrix where
  xmlTagName _ = "feConvolveMatrix"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [ "in" `parseIn` convolveMatrixIn,
      "order" `parseIn` convolveMatrixOrder,
      "kernelMatrix" `parseIn` convolveMatrixKernelMatrix,
      "divisor" `parseIn` convolveMatrixDivisor,
      "bias" `parseIn` convolveMatrixBias,
      "targetX" `parseIn` convolveMatrixTargetX,
      "targetY" `parseIn` convolveMatrixTargetY,
      "edgeMode" `parseIn` convolveMatrixEdgeMode,
      "preserveAlpha" `parseIn` convolveMatrixPreserveAlpha ]

instance XMLUpdatable SpecularLighting where
  xmlTagName _ = "feSpecularLighting"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [ "in" `parseIn` specLightingIn,
      "surfaceScale" `parseIn` specLightingSurfaceScale,
      "specularConstant" `parseIn` specLightingSpecularConst,
      "specularExponent" `parseIn` specLightingSpecularExp,
      "kernelUnitLength" `parseIn` specLightingKernelUnitLength ]

instance XMLUpdatable DiffuseLighting where
  xmlTagName _ = "feDiffuseLighting"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [ "in" `parseIn` diffuseLightingIn,
      "surfaceScale" `parseIn` diffuseLightingSurfaceScale,
      "diffuseConstant" `parseIn` diffuseLightingDiffuseConst,
      "kernelUnitLength" `parseIn` diffuseLightingKernelUnitLength]

instance XMLUpdatable Morphology where
  xmlTagName _ = "feMorphology"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [ "in" `parseIn` morphologyIn,
      "operator" `parseIn` morphologyOperator,
      "radius" `parseIn` morphologyRadius ]

instance XMLUpdatable DropShadow where
  xmlTagName _ = "feDropShadow"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [ "dx" `parseIn` dropShadowDx,
      "dy" `parseIn` dropShadowDy,
      "stdDeviation" `parseIn` dropShadowStdDeviation ]

instance XMLUpdatable Blend where
  xmlTagName _ = "feBlend"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [ "in" `parseIn` blendIn
    , "in2" `parseIn` blendIn2
    , "mode"  `parseIn` blendMode ]

instance XMLUpdatable FuncA where
  xmlTagName _ = "feFuncA"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [ "type" `parseIn` funcAType
    , "tableValues" `parseIn` funcATableValues
    , "slope" `parseIn` funcASlope
    , "intercept" `parseIn` funcAIntercept
    , "amplitude" `parseIn` funcAAmplitude
    , "exponent" `parseIn` funcAExponent ]

instance XMLUpdatable FuncR where
  xmlTagName _ = "feFuncR"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [ "type" `parseIn` funcRType
    , "tableValues" `parseIn` funcRTableValues
    , "slope" `parseIn` funcRSlope
    , "intercept" `parseIn` funcRIntercept
    , "amplitude" `parseIn` funcRAmplitude
    , "exponent" `parseIn` funcRExponent ]

instance XMLUpdatable FuncG where
  xmlTagName _ = "feFuncG"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [ "type" `parseIn` funcGType
    , "tableValues" `parseIn` funcGTableValues
    , "slope" `parseIn` funcGSlope
    , "intercept" `parseIn` funcGIntercept
    , "amplitude" `parseIn` funcGAmplitude
    , "exponent" `parseIn` funcGExponent ]

instance XMLUpdatable FuncB where
  xmlTagName _ = "feFuncB"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [ "type" `parseIn` funcBType
    , "tableValues" `parseIn` funcBTableValues
    , "slope" `parseIn` funcBSlope
    , "intercept" `parseIn` funcBIntercept
    , "amplitude" `parseIn` funcBAmplitude
    , "exponent" `parseIn` funcBExponent ]

instance XMLUpdatable Flood where
  xmlTagName _ = "feFlood"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [ "flood-color" `parseIn` floodColor
    , "flood-opacity" `parseIn` floodOpacity]

instance XMLUpdatable Tile where
  xmlTagName _ = "feTile"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [ "in" `parseIn` tileIn]

instance XMLUpdatable Offset where
  xmlTagName _ = "feOffset"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [ "in" `parseIn` offsetIn
    , "dx" `parseIn` offsetDX
    , "dy" `parseIn` offsetDY ]

instance XMLUpdatable Merge where
  xmlTagName _ = "feMerge"
  serializeTreeNode node =
     updateWithAccessor _mergeChildren node $
        genericSerializeWithDrawAttr node
  attributes = []

instance XMLUpdatable ComponentTransfer where
  xmlTagName _ = "feComponentTransfer"
  serializeTreeNode node =
     updateWithAccessor _compTransferChildren node $
        genericSerializeWithDrawAttr node
  attributes =
    [ "in" `parseIn` compTransferIn ]


instance XMLUpdatable MergeNode where
  xmlTagName _ = "feMergeNode"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [ "in" `parseIn` mergeNodeIn ]

instance XMLUpdatable ImageF where
  xmlTagName _ = "feImage"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [ --parserSetter "href" imageFHref (Just . dropSharp) Just
      "href" `parseIn` imageFHref
    , "preserveAspectRatio" `parseIn` imageFAspectRatio
    ]

instance XMLUpdatable ColorMatrix where
  xmlTagName _ = "feColorMatrix"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [ "in" `parseIn` colorMatrixIn
    , "type" `parseIn` colorMatrixType
    , "values" `parseIn` colorMatrixValues ]

instance XMLUpdatable Composite where
  xmlTagName _ = "feComposite"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [ "in" `parseIn` compositeIn
    , "in2" `parseIn` compositeIn2
    , "operator" `parseIn` compositeOperator
    , "k1" `parseIn` compositeK1
    , "k2" `parseIn` compositeK2
    , "k3" `parseIn` compositeK3
    , "k4" `parseIn` compositeK4 ]

instance XMLUpdatable GaussianBlur where
  xmlTagName _ = "feGaussianBlur"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [ "in" `parseIn` gaussianBlurIn
    , "stdDeviation" `parseIn` lensProduct gaussianBlurStdDeviationX gaussianBlurStdDeviationY
    , "edgeMode" `parseIn` gaussianBlurEdgeMode ]

instance XMLUpdatable DisplacementMap where
  xmlTagName _ = "feDisplacementMap"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [ "in" `parseIn` displacementMapIn
    , "in2" `parseIn` displacementMapIn2
    , "scale" `parseIn` displacementMapScale
    , "xChannelSelector" `parseIn` displacementMapXChannelSelector
    , "yChannelSelector" `parseIn` displacementMapYChannelSelector ]

instance XMLUpdatable Turbulence where
  xmlTagName _ = "feTurbulence"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    [ "baseFrequency" `parseIn` turbulenceBaseFrequency
    , "numOctaves" `parseIn` turbulenceNumOctaves
    , "seed" `parseIn` turbulenceSeed
    , "stitchTiles" `parseIn` turbulenceStitchTiles
    , "type" `parseIn` turbulenceType ]

instance XMLUpdatable RadialGradient where
  xmlTagName _ = "radialGradient"
  serializeTreeNode node =
     updateWithAccessor _radialGradientStops node $ genericSerializeWithDrawAttr node
  attributes =
    ["gradientTransform" `parseIn` radialGradientTransform
    ,"gradientUnits" `parseIn` radialGradientUnits
    ,"spreadMethod" `parseIn` radialGradientSpread
    ,"cx" `parseIn` (radialGradientCenter._1)
    ,"cy" `parseIn` (radialGradientCenter._2)
    ,"r"  `parseIn` radialGradientRadius
    ,"fx" `parseIn` radialGradientFocusX
    ,"fy" `parseIn` radialGradientFocusY
    ]

instance XMLUpdatable Use where
  xmlTagName _ = "use"
  serializeTreeNode = genericSerializeWithDrawAttr
  attributes =
    ["x" `parseIn` (useBase._1)
    ,"y" `parseIn` (useBase._2)
    ,"width" `parseIn` useWidth
    ,"height" `parseIn` useHeight
    ,parserSetter "href" useName (Just . dropSharp) (Just . ('#':))
    ]

dropSharp :: String -> String
dropSharp ('#':rest) = rest
dropSharp a          = a

instance XMLUpdatable TextInfo where
  xmlTagName _ = "tspan"
  serializeTreeNode = genericSerializeNode
  attributes =
    [parserSetter "x" textInfoX (parse dashArray) dashNotEmpty
    ,parserSetter "y" textInfoY (parse dashArray) dashNotEmpty
    ,parserSetter "dx" textInfoDX (parse dashArray) dashNotEmpty
    ,parserSetter "dy" textInfoDY (parse dashArray) dashNotEmpty
    ,parserSetter "rotate" textInfoRotate
        (parse numberList)
        rotateNotEmpty
    ,"textLength" `parseIn` textInfoLength
    ]
    where
      dashNotEmpty []  = Nothing
      dashNotEmpty lst = Just $ serializeDashArray lst

      rotateNotEmpty [] = Nothing
      rotateNotEmpty lst =
          Just . unwords $ printf "%s" . ppD <$> lst


instance XMLUpdatable TextPath where
  xmlTagName _ =  "textPath"
  serializeTreeNode = genericSerializeNode
  attributes =
    ["startOffset" `parseIn` textPathStartOffset
    ,"method" `parseIn` textPathMethod
    ,"spacing" `parseIn` textPathSpacing
    ,parserSetter "href" textPathName (Just . dropSharp) (Just . ('#':))
    ]

instance XMLUpdatable Text where
  xmlTagName _ = "text"
  serializeTreeNode = serializeText
  attributes = ["lengthAdjust" `parseIn` textAdjust]


instance XMLUpdatable Pattern where
  xmlTagName _ = "pattern"
  serializeTreeNode node =
     updateWithAccessor _patternElements node $ genericSerializeWithDrawAttr node
  attributes =
    ["viewBox" `parseIn` patternViewBox
    ,"patternUnits" `parseIn` patternUnit
    ,"width" `parseIn` patternWidth
    ,"height" `parseIn` patternHeight
    ,"x" `parseIn` (patternPos._1)
    ,"y" `parseIn` (patternPos._2)
    ,"preserveAspectRatio" `parseIn` patternAspectRatio
    ,parserSetter "href" patternHref (Just . dropSharp) (Just . ('#':))
    ,"patternTransform" `parseIn` patternTransform
    ]

instance XMLUpdatable Marker where
  xmlTagName _ = "marker"
  serializeTreeNode node =
     updateWithAccessor _markerElements node $ genericSerializeWithDrawAttr node
  attributes =
    ["refX" `parseIn` (markerRefPoint._1)
    ,"refY" `parseIn` (markerRefPoint._2)
    ,"markerWidth" `parseIn` markerWidth
    ,"markerHeight" `parseIn` markerHeight
    ,"patternUnits" `parseIn` markerUnits
    ,"orient" `parseIn` markerOrient
    ,"viewBox" `parseIn` markerViewBox
    ,"overflow" `parseIn` markerOverflow
    ,"preserveAspectRatio" `parseIn` markerAspectRatio
    ]

serializeText :: Text -> Maybe X.Element
serializeText topText = namedNode where
  namedNode = fmap (\x -> x { X.elName = X.unqual "text" }) topNode
  topNode = serializeSpan $ _textRoot topText

  serializeSpan tspan = case (info, drawInfo) of
    (Nothing, Nothing) -> Nothing
    (Just a, Nothing) -> Just $ setChildren a subContent
    (Nothing, Just b) -> Just $ setChildren b subContent
    (Just a, Just b) ->
        Just $ setChildren (mergeAttributes a b) subContent
    where
      info = genericSerializeNode $ _spanInfo tspan
      drawInfo = genericSerializeNode $ _spanDrawAttributes tspan
      subContent = catMaybes $ serializeContent <$> _spanContent tspan

  serializeContent (SpanText t) = Just . X.Text $ X.blank_cdata { X.cdData = T.unpack t }
  serializeContent (SpanTextRef _t) = Just . X.Text $ X.blank_cdata { X.cdData = "" }
  serializeContent (SpanSub sub) = X.Elem <$> serializeSpan sub

unparseText :: [X.Content] -> ([TextSpanContent], Maybe TextPath)
unparseText = extractResult . go True
  where
    extractResult (a, b, _) = (a, b)

    go startStrip [] = ([], Nothing, startStrip)
    go startStrip (X.CRef _:rest) = go startStrip rest
    go startStrip (X.Elem e@(nodeName -> "tspan"):rest) =
        (SpanSub spans : trest, mpath, retStrip)
      where
        (trest, mpath, retStrip) = go restStrip rest
        (sub, _, restStrip) = go startStrip $ X.elContent e
        spans = TextSpan (xmlUnparse e) (xmlUnparse e) sub

    go startStrip (X.Elem e@(nodeName -> "tref"):rest) =
        case attributeFinder "href" e of
          Nothing -> go startStrip rest
          Just v -> (SpanTextRef v : trest, mpath, stripRet)
            where (trest, mpath, stripRet) = go startStrip rest

    go startStrip (X.Elem e@(nodeName -> "textPath"):rest) =
        case attributeFinder "href" e of
          Nothing -> go startStrip rest
          Just v -> (tsub ++ trest, pure p, retStrp)
            where
              p = (xmlUnparse e) { _textPathName = dropSharp v }
              (trest, _, retStrp) = go restStrip rest
              (tsub, _, restStrip) = go startStrip $ X.elContent e

    go startStrip (X.Elem _:rest) = go startStrip rest
    go startStrip (X.Text t:rest)
      | T.length cleanText == 0 = go startStrip rest
      | otherwise =
        (SpanText cleanText : trest, mpath, stripRet)
       where
         (trest, mpath, stripRet) = go subShouldStrip rest

         subShouldStrip = T.pack " " `T.isSuffixOf` cleanText

         space = T.singleton ' '
         singulariseSpaces tt
            | space `T.isPrefixOf` tt = space
            | otherwise = tt

         stripStart | startStrip = T.stripStart
                    | otherwise = id

         cleanText = stripStart
                   . T.concat
                   . fmap singulariseSpaces
                   . T.groupBy (\a b -> (a /= ' ' && b /= ' ') || a == b)
                   . T.filter (\c -> c /= '\n' && c /= '\r')
                   . T.map (\c -> if c == '\t' then ' ' else c)
                   . T.pack
                   $ X.cdData t

gradientOffsetSetter :: SvgAttributeLens GradientStop
gradientOffsetSetter = SvgAttributeLens "offset" setter serialize
  where
    serialize a = Just $ printf "%d%%" percentage
      where percentage = floor . (100 *) $ a ^. gradientOffset :: Int

    setter el str = el & gradientOffset .~ val
      where
        val = realToFrac $ case parseMayStartDot complexNumber str of
            Nothing          -> 0
            Just (Num n)     -> n
            Just (Px n)      -> n
            Just (Percent n) -> n
            Just (Em n)      -> n
            Just (Pc n)      -> n
            Just (Mm n)      -> n
            Just (Cm n)      -> n
            Just (Point n)   -> n
            Just (Inches n)  -> n

instance XMLUpdatable GradientStop where
    xmlTagName _ = "stop"
    serializeTreeNode = genericSerializeNode
    attributes = styleAttribute cssAvailable : fmap fst cssAvailable ++ lst where
      cssAvailable :: [(SvgAttributeLens GradientStop, CssUpdater GradientStop)]
      cssAvailable =
          [(opacitySetter "stop-opacity" gradientOpacity, cssUniqueFloat gradientOpacity)
          ,("stop-color" `parseIn` gradientColor, cssUniqueColor gradientColor)
          ]

      lst =
        [gradientOffsetSetter
        ,"path" `parseIn` gradientPath
        ]


parseGradientStops :: X.Element -> [GradientStop]
parseGradientStops = concatMap unStop . elChildren
  where
    unStop e@(nodeName -> "stop") = [xmlUnparse e]
    unStop _                      = []

parseMeshGradientPatches :: X.Element -> [MeshGradientPatch]
parseMeshGradientPatches = foldMap unparsePatch . elChildren where
  unparsePatch e@(nodeName -> "meshpatch") = [MeshGradientPatch $ parseGradientStops e]
  unparsePatch _ = []

parseMeshGradientRows :: X.Element -> [MeshGradientRow]
parseMeshGradientRows = foldMap unRows . elChildren where
  unRows e@(nodeName -> "meshrow") = [MeshGradientRow $ parseMeshGradientPatches e]
  unRows _ = []

-- This is to guarantee there will be only "feMergeNode" elements inside any "feMerge" element.
unparseMergeNode :: X.Element -> FilterElement
unparseMergeNode e@(nodeName -> "feMergeNode") =
  FEMergeNode $ xmlUnparseWithDrawAttr e
unparseMergeNode _ = FENone

-- This is to guarantee there will be only "feFunc_" elements inside any "feComponentTransfer" element.
unparseFunc :: X.Element -> FilterElement
unparseFunc e = case nodeName e of
  "feFuncA" -> FEFuncA $ xmlUnparseWithDrawAttr e
  "feFuncR" -> FEFuncR $ xmlUnparseWithDrawAttr e
  "feFuncG" -> FEFuncG $ xmlUnparseWithDrawAttr e
  "feFuncB" -> FEFuncB $ xmlUnparseWithDrawAttr e
  _         -> FENone

unparseFE :: X.Element -> FilterElement
unparseFE e = flip xmlUpdate e $
  case nodeName e of
    "feMerge" ->
      FEMerge $ xmlUnparseWithDrawAttr e
        & mergeChildren .~ map unparseMergeNode (elChildren e)
    "feComponentTransfer" ->
      FEComponentTransfer $ xmlUnparseWithDrawAttr e
        & compTransferChildren .~ map unparseFunc (elChildren e)
    "feBlend"            -> FEBlend parsed
    "feColorMatrix"      -> FEColorMatrix parsed
    "feComposite"        -> FEComposite parsed
    "feDisplacementMap"  -> FEDisplacementMap parsed
    "feGaussianBlur"     -> FEGaussianBlur parsed
    "feTurbulence"       -> FETurbulence parsed
    "feTile"             -> FETile parsed
    "feFlood"            -> FEFlood parsed
    "feOffset"           -> FEOffset parsed
    "feImage"            -> FEImage parsed
    "feMergeNode"        -> FEMergeNode parsed -- Potential bug: allow the "feMergeNode" element to appear outside a "feMerge" element.
    "feFuncA"            -> FEFuncA parsed -- Potential bug: allow the "feFuncA" element to appear outside a "feComponentTransfer" element.
    "feFuncR"            -> FEFuncR parsed -- Potential bug: allow the "feFuncR" element to appear outside a "feComponentTransfer" element.
    "feFuncG"            -> FEFuncG parsed -- Potential bug: allow the "feFuncG" element to appear outside a "feComponentTransfer" element.
    "feFuncB"            -> FEFuncB parsed -- Potential bug: allow the "feFuncB" element to appear outside a "feComponentTransfer" element.
    "feSpecularLighting" -> FESpecularLighting parsed
    "feConvolveMatrix"   -> FEConvolveMatrix parsed
    "feDiffuseLighting"  -> FEDiffuseLighting parsed
    "feMorphology"       -> FEMorphology parsed
    "feDropShadow"       -> FEDropShadow parsed
    _                    -> FENone
  where
    parsed :: (WithDefaultSvg a, XMLUpdatable a, HasDrawAttributes a) => a
    parsed = xmlUnparseWithDrawAttr e

unparse :: X.Element -> Tree
unparse e@(nodeName -> "pattern") =
  PatternTree $ xmlUnparse e & patternElements .~ map unparse (elChildren e)
unparse e@(nodeName -> "marker") =
  MarkerTree $ xmlUnparseWithDrawAttr e & markerElements .~ map unparse (elChildren e)
unparse e@(nodeName -> "mask") =
  MaskTree $ xmlUnparseWithDrawAttr e & maskContent .~ map unparse (elChildren e)
unparse e@(nodeName -> "clipPath") =
  ClipPathTree $ xmlUnparseWithDrawAttr e & clipPathContent .~ map unparse (elChildren e)
unparse (nodeName -> "style") = None -- XXX: Create a style node?
unparse e@(nodeName -> "defs") =
  DefinitionTree $ xmlUnparseWithDrawAttr e & groupChildren .~ map unparse (elChildren e)
unparse e@(nodeName -> "filter") =
  FilterTree $ xmlUnparseWithDrawAttr e & filterChildren .~ map unparseFE (elChildren e)
unparse e@(nodeName -> "symbol") =
  SymbolTree $ xmlUnparseWithDrawAttr e & groupChildren .~ map unparse (elChildren e)
unparse e@(nodeName -> "g") =
  GroupTree $ xmlUnparseWithDrawAttr e & groupChildren .~ map unparse (elChildren e)
unparse e@(nodeName -> "svg") =
  maybe None SvgTree $ unparseDocument "" e
unparse e@(nodeName -> "text") =
  TextTree tPath $ xmlUnparse e & textRoot .~ root
    where
      (textContent, tPath) = unparseText $ X.elContent e

      root = TextSpan
           { _spanInfo = xmlUnparse e
           , _spanDrawAttributes = xmlUnparse e
           , _spanContent = textContent
           }

unparse e = case nodeName e of
    "image"    -> ImageTree parsed
    "ellipse"  -> EllipseTree parsed
    "rect"     -> RectangleTree parsed
    "polyline" -> PolyLineTree parsed
    "polygon"  -> PolygonTree parsed
    "circle"   -> CircleTree parsed
    "line"     -> LineTree parsed
    "path"     -> PathTree parsed
    "linearGradient" ->
      LinearGradientTree $ parsed & linearGradientStops .~ parseGradientStops e
    "radialGradient" ->
      RadialGradientTree $ parsed & radialGradientStops .~ parseGradientStops e
    "meshgradient" ->
      MeshGradientTree $ parsed & meshGradientRows .~ parseMeshGradientRows e
    "use" -> UseTree parsed Nothing
    _ -> None
  where
    parsed :: (WithDefaultSvg a, XMLUpdatable a, HasDrawAttributes a) => a
    parsed = xmlUnparseWithDrawAttr e

unparseDocument :: FilePath -> X.Element -> Maybe Document
unparseDocument rootLocation e@(nodeName -> "svg") = Just Document
    { _documentViewBox =
        attributeFinder "viewBox" e >>= parse viewBoxParser
    , _documentElements = parsedElements
    , _documentWidth = lengthFind "width"
    , _documentHeight = lengthFind "height"
    , _documentDescription = ""
    , _documentLocation = rootLocation
    , _documentAspectRatio =
        fromMaybe defaultSvg $
        attributeFinder "preserveAspectRatio" e >>= aparse
    }
  where
    parsedElements = map unparse $ elChildren e
    lengthFind n =
        attributeFinder n e >>= parse complexNumber
unparseDocument _ _ = Nothing

-- | Transform a SVG document to a XML node.
xmlOfDocument :: Document -> X.Element
xmlOfDocument doc =
    X.node (X.unqual "svg") (attrs, descTag ++ children)
  where
    attr name = X.Attr (X.unqual name)
    children = catMaybes [serializeTreeNode el | el <- _documentElements doc]

    docViewBox = case _documentViewBox doc of
        Nothing -> []
        Just b  -> [attr "viewBox" $ serializeViewBox b]

    descTag = case _documentDescription doc of
        ""  -> []
        txt -> [X.node (X.unqual "desc") txt]

    attrs =
        docViewBox ++
        [attr "xmlns" "http://www.w3.org/2000/svg"
        ,attr "xmlns:xlink" "http://www.w3.org/1999/xlink"
        ,attr "version" "1.1"] ++
        catMaybes [attr "width" . serializeNumber <$> _documentWidth doc
                  ,attr "height" . serializeNumber <$> _documentHeight doc
                  ] ++
        catMaybes [attr "preserveAspectRatio" <$>  aserialize (_documentAspectRatio doc)
                  | _documentAspectRatio doc /= defaultSvg ]

xmlOfTree :: Tree -> Maybe X.Element
xmlOfTree = serializeTreeNode
