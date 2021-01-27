{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module define all the types used in the definition
-- of a svg scene.
--
-- Most of the types are lensified.
module Graphics.SvgTree.Types.Internal
  ( -- * Basic building types
    Coord,
    Origin (..),
    Point,
    RPoint,
    PathCommand (..),
    Transformation (..),
    ElementRef (..),
    CoordinateUnits (..),

    -- ** Building helpers
    serializeNumber,
    serializeTransformation,
    serializeTransformations,

    -- * Drawing control types
    Cap (..),
    LineJoin (..),
    Tree (..),
    TreeBranch (..),
    Number (..),
    Spread (..),
    Texture (..),
    Element (..),
    FillRule (..),
    FontStyle (..),
    Dpi,
    WithDefaultSvg (..),

    -- * Main type
    Document (..),
    documentViewBox,
    documentWidth,
    documentHeight,
    documentElements,
    documentDescription,
    documentLocation,
    documentAspectRatio,
    documentSize,

    -- * Drawing attributes
    DrawAttributes (..),
    HasDrawAttributes (..),

    -- * Filters
    FilterElement (..),
    FilterAttributes (..),
    HasFilterAttributes (..),
    FilterSource (..),

    Blend (..),
    BlendMode (..),
    blendDrawAttributes,
    blendFilterAttr,
    blendIn,
    blendIn2,
    blendMode,

    ConvolveMatrix (..),
    convolveMatrixDrawAttributes,
    convolveMatrixFilterAttr,
    convolveMatrixIn,
    convolveMatrixOrder,
    convolveMatrixKernelMatrix,
    convolveMatrixDivisor,
    convolveMatrixBias,
    convolveMatrixTargetX,
    convolveMatrixTargetY,
    convolveMatrixEdgeMode,
    convolveMatrixKernelUnitLength,
    convolveMatrixPreserveAlpha,

    Morphology (..),
    OperatorType (..),
    NumberOptionalNumber (..),
    morphologyDrawAttributes,
    morphologyFilterAttr,
    morphologyIn,
    morphologyOperator,
    morphologyRadius,

    SpecularLighting (..),
    specLightingDrawAttributes,
    specLightingFilterAttr,
    specLightingIn,
    specLightingSurfaceScale,
    specLightingSpecularConst,
    specLightingSpecularExp,
    specLightingKernelUnitLength,

    DropShadow (..),
    dropShadowDrawAttributes,
    dropShadowFilterAttr,
    dropShadowDx,
    dropShadowDy,
    dropShadowStdDeviation,

    DiffuseLighting,
    diffuseLightingDrawAttributes,
    diffuseLightingFilterAttr,
    diffuseLightingIn,
    diffuseLightingSurfaceScale,
    diffuseLightingDiffuseConst,
    diffuseLightingKernelUnitLength,

    Tile (..),
    tileDrawAttributes,
    tileFilterAttr,
    tileIn,

    Flood (..),
    floodDrawAttributes,
    floodFilterAttr,
    floodColor,
    floodOpacity,

    Offset (..),
    offsetDrawAttributes,
    offsetFilterAttr,
    offsetIn,
    offsetDX,
    offsetDY,

    Merge (..),
    mergeDrawAttributes,
    mergeFilterAttributes,
    mergeChildren,

    MergeNode (..),
    mergeNodeDrawAttributes,
    mergeNodeIn,

    ImageF (..),
    imageFDrawAttributes,
    imageFFilterAttr,
    imageFHref,
    imageFAspectRatio,

    ComponentTransfer (..),
    compTransferDrawAttributes,
    compTransferFilterAttr,
    compTransferChildren,
    compTransferIn,

    FuncA (..),
    FuncType (..),
    funcADrawAttributes,
    funcAType,
    funcATableValues,
    funcASlope,
    funcAIntercept,
    funcAAmplitude,
    funcAExponent,

    FuncR (..),
    funcRDrawAttributes,
    funcRType,
    funcRTableValues,
    funcRSlope,
    funcRIntercept,
    funcRAmplitude,
    funcRExponent,

    FuncG (..),
    funcGDrawAttributes,
    funcGType,
    funcGTableValues,
    funcGSlope,
    funcGIntercept,
    funcGAmplitude,
    funcGExponent,

    FuncB (..),
    funcBDrawAttributes,
    funcBType,
    funcBTableValues,
    funcBSlope,
    funcBIntercept,
    funcBAmplitude,
    funcBExponent,

    ColorMatrixType (..),
    colorMatrixDrawAttributes,
    colorMatrixFilterAttr,
    colorMatrixIn,
    colorMatrixType,
    colorMatrixValues,
    ColorMatrix (..),
    compositeDrawAttributes,
    compositeFilterAttr,
    compositeIn,
    compositeIn2,
    compositeOperator,
    compositeK1,
    compositeK2,
    compositeK3,
    compositeK4,
    Composite (..),
    CompositeOperator (..),
    EdgeMode (..),
    gaussianBlurDrawAttributes,
    gaussianBlurFilterAttr,
    gaussianBlurIn,
    gaussianBlurStdDeviationX,
    gaussianBlurStdDeviationY,
    gaussianBlurEdgeMode,
    GaussianBlur (..),
    turbulenceDrawAttributes,
    turbulenceFilterAttr,
    turbulenceBaseFrequency,
    turbulenceNumOctaves,
    turbulenceSeed,
    turbulenceStitchTiles,
    turbulenceType,
    Turbulence (..),
    TurbulenceType (..),
    StitchTiles (..),
    DisplacementMap (..),
    displacementMapDrawAttributes,
    displacementMapFilterAttr,
    displacementMapIn,
    displacementMapIn2,
    displacementMapScale,
    displacementMapXChannelSelector,
    displacementMapYChannelSelector,
    ChannelSelector (..),

    -- * SVG drawing primitives

    -- ** Rectangle
    Rectangle (..),
    rectangleDrawAttributes,
    rectUpperLeftCorner,
    rectWidth,
    rectHeight,
    rectCornerRadius,

    -- ** Line
    Line (..),
    lineDrawAttributes,
    linePoint1,
    linePoint2,

    -- ** Polygon
    Polygon (..),
    polygonDrawAttributes,
    polygonPoints,

    -- ** Polyline
    PolyLine (..),
    polyLineDrawAttributes,
    polyLinePoints,

    -- ** Path
    Path (..),
    pathDrawAttributes,
    pathDefinition,

    -- ** Circle
    Circle (..),
    circleDrawAttributes,
    circleCenter,
    circleRadius,

    -- ** Ellipse
    Ellipse (..),
    ellipseDrawAttributes,
    ellipseCenter,
    ellipseXRadius,
    ellipseYRadius,

    -- ** Mesh (gradient mesh)
    GradientPathCommand (..),
    MeshGradientType (..),
    MeshGradient (..),
    meshGradientDrawAttributes,
    meshGradientX,
    meshGradientY,
    meshGradientType,
    meshGradientUnits,
    meshGradientTransform,
    meshGradientRows,
    MeshGradientRow (..),
    meshGradientRowPatches,
    MeshGradientPatch (..),
    meshGradientPatchStops,

    -- ** Image
    Image (..),
    imageDrawAttributes,
    imageCornerUpperLeft,
    imageWidth,
    imageHeight,
    imageHref,
    imageAspectRatio,

    -- ** Use
    Use (..),
    useDrawAttributes,
    useBase,
    useName,
    useWidth,
    useHeight,

    -- * Grouping primitives

    -- ** Group
    Group (..),
    groupDrawAttributes,
    groupChildren,
    groupViewBox,
    groupAspectRatio,

    -- ** Filter
    Filter (..),
    filterDrawAttributes,
    filterSelfAttributes,
    filterChildren,

    -- * Text related types

    -- ** Text
    Text (..),
    textAdjust,
    textRoot,
    TextAnchor (..),
    textAt,

    -- ** Text path
    TextPath (..),
    textPathStartOffset,
    textPathName,
    textPathMethod,
    textPathSpacing,
    TextPathSpacing (..),
    TextPathMethod (..),

    -- ** Text span.
    TextSpanContent (..),
    TextSpan (..),
    spanInfo,
    spanDrawAttributes,
    spanContent,
    TextInfo (..),
    textInfoX,
    textInfoY,
    textInfoDX,
    textInfoDY,
    textInfoRotate,
    textInfoLength,
    TextAdjust (..),

    -- * Marker definition
    Marker (..),
    Overflow (..),
    MarkerOrientation (..),
    MarkerUnit (..),
    markerDrawAttributes,
    markerRefPoint,
    markerWidth,
    markerHeight,
    markerOrient,
    markerUnits,
    markerViewBox,
    markerOverflow,
    markerAspectRatio,
    markerElements,

    -- * Gradient definition
    GradientStop (..),
    gradientOffset,
    gradientColor,
    gradientPath,
    gradientOpacity,

    -- ** Linear Gradient
    LinearGradient (..),
    linearGradientDrawAttributes,
    linearGradientUnits,
    linearGradientStart,
    linearGradientStop,
    linearGradientSpread,
    linearGradientTransform,
    linearGradientStops,

    -- ** Radial Gradient
    RadialGradient (..),
    radialGradientDrawAttributes,
    radialGradientUnits,
    radialGradientCenter,
    radialGradientRadius,
    radialGradientFocusX,
    radialGradientFocusY,
    radialGradientSpread,
    radialGradientTransform,
    radialGradientStops,

    -- * Pattern definition
    Pattern (..),
    patternDrawAttributes,
    patternViewBox,
    patternWidth,
    patternHeight,
    patternPos,
    patternHref,
    patternElements,
    patternUnit,
    patternAspectRatio,
    patternTransform,

    -- * Mask definition
    Mask (..),
    maskDrawAttributes,
    maskContentUnits,
    maskUnits,
    maskPosition,
    maskWidth,
    maskHeight,
    maskContent,

    -- * Clip path definition
    ClipPath (..),
    clipPathDrawAttributes,
    clipPathUnits,
    clipPathContent,

    -- * Aspect Ratio description
    PreserveAspectRatio (..),
    Alignment (..),
    MeetSlice (..),
    aspectRatioDefer,
    aspectRatioAlign,
    aspectRatioMeetSlice,

    -- * MISC functions
    nameOfTree,
    toUserUnit,
    mapNumber,
  )
where

import Codec.Picture (PixelRGBA8 (..))
import Control.Applicative ((<|>))
import Control.Lens.TH (makeClassy, makeLenses)
import Data.Function (on)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Graphics.SvgTree.CssTypes
  ( Dpi,
    Number (..),
    mapNumber,
    serializeNumber,
    toUserUnit,
  )
import Graphics.SvgTree.Misc (ppD)
import Graphics.SvgTree.Types.Basic
import Text.Printf (printf)

-- | Path command definition.
data PathCommand
  = -- | 'M' or 'm' command
    MoveTo !Origin ![RPoint]
  | -- | Line to, 'L' or 'l' Svg path command.
    LineTo !Origin ![RPoint]
  | -- | Equivalent to the 'H' or 'h' svg path command.
    HorizontalTo !Origin ![Coord]
  | -- | Equivalent to the 'V' or 'v' svg path command.
    VerticalTo !Origin ![Coord]
  | -- | Cubic bezier, 'C' or 'c' command
    CurveTo !Origin ![(RPoint, RPoint, RPoint)]
  | -- | Smooth cubic bezier, equivalent to 'S' or 's' command
    SmoothCurveTo !Origin ![(RPoint, RPoint)]
  | -- | Quadratic bezier, 'Q' or 'q' command
    QuadraticBezier !Origin ![(RPoint, RPoint)]
  | -- | Quadratic bezier, 'T' or 't' command
    SmoothQuadraticBezierCurveTo !Origin ![RPoint]
  | -- | Eliptical arc, 'A' or 'a' command.
    EllipticalArc !Origin ![(Coord, Coord, Coord, Bool, Bool, RPoint)]
  | -- | Close the path, 'Z' or 'z' svg path command.
    EndPath
  deriving (Eq, Show, Generic)

-- | Description of path used in meshgradient tag
data GradientPathCommand
  = -- | Line to, 'L' or 'l' Svg path command.
    GLine !Origin !(Maybe RPoint)
  | -- | Cubic bezier, 'C' or 'c' command
    GCurve !Origin !RPoint !RPoint !(Maybe RPoint)
  | -- | 'Z' command
    GClose
  deriving (Eq, Show, Generic)

-- | Describe the content of the preserveAspectRatio attribute.
data PreserveAspectRatio = PreserveAspectRatio
  { _aspectRatioDefer :: !Bool,
    _aspectRatioAlign :: !Alignment,
    _aspectRatioMeetSlice :: !(Maybe MeetSlice)
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg PreserveAspectRatio where
  defaultSvg =
    PreserveAspectRatio
      { _aspectRatioDefer = False,
        _aspectRatioAlign = AlignxMidYMid,
        _aspectRatioMeetSlice = Nothing
      }

-- | Describe the content of the `transformation` attribute.
-- see `_transform` and `transform`.
data Transformation
  = -- | Directly encode the translation matrix.
    TransformMatrix
      !Coord
      !Coord
      !Coord
      !Coord
      !Coord
      !Coord
  | -- | Translation along a vector
    Translate !Double !Double
  | -- | Scaling on both axis or on X axis and Y axis.
    Scale !Double !(Maybe Double)
  | -- | Rotation around `(0, 0)` or around an optional
    -- point.
    Rotate !Double !(Maybe (Double, Double))
  | -- | Skew transformation along the X axis.
    SkewX !Double
  | -- | Skew transformation along the Y axis.
    SkewY !Double
  | -- | Unkown transformation, like identity.
    TransformUnknown
  deriving (Eq, Show, Generic)

-- | Convert the Transformation to a string which can be
-- directly used in a svg attributes.
serializeTransformation :: Int -> Transformation -> String
serializeTransformation precision t = case t of
  TransformUnknown -> ""
  TransformMatrix a b c d e f ->
    printf
      "matrix(%s, %s, %s, %s, %s, %s)"
      (ppD precision a)
      (ppD precision b)
      (ppD precision c)
      (ppD precision d)
      (ppD precision e)
      (ppD precision f)
  Translate x y -> printf "translate(%s, %s)" (ppD precision x) (ppD precision y)
  Scale x Nothing -> printf "scale(%s)" (ppD precision x)
  Scale x (Just y) -> printf "scale(%s, %s)" (ppD precision x) (ppD precision y)
  Rotate angle Nothing -> printf "rotate(%s)" (ppD precision angle)
  Rotate angle (Just (x, y)) ->
    printf
      "rotate(%s, %s, %s)"
      (ppD precision angle)
      (ppD precision x)
      (ppD precision y)
  SkewX x -> printf "skewX(%s)" (ppD precision x)
  SkewY y -> printf "skewY(%s)" (ppD precision y)

-- | Transform a list of transformations to a string for svg
-- `transform` attributes.
serializeTransformations :: Int -> [Transformation] -> String
serializeTransformations precision =
  unwords . fmap (serializeTransformation precision)

-- | Define an empty 'default' element for the SVG tree.
-- It is used as base when parsing the element from XML.
class WithDefaultSvg a where
  -- | The default element.
  defaultSvg :: a

-- | Classify the font style, used to search a matching
-- font in the FontCache.
data FontStyle
  = FontStyleNormal
  | FontStyleItalic
  | FontStyleOblique
  deriving (Eq, Show, Generic)

-- | Tell where to anchor the text, where the position
-- given is realative to the text.
data TextAnchor
  = -- | The text with left aligned, or start at the postion
    -- If the point is the '*' then the text will be printed
    -- this way:
    --
    -- >  *THE_TEXT_TO_PRINT
    --
    -- Equivalent to the `start` value.
    TextAnchorStart
  | -- | The text is middle aligned, so the text will be at
    -- the left and right of the position:
    --
    -- >   THE_TEXT*TO_PRINT
    --
    -- Equivalent to the `middle` value.
    TextAnchorMiddle
  | -- | The text is right aligned.
    --
    -- >   THE_TEXT_TO_PRINT*
    --
    -- Equivalent to the `end` value.
    TextAnchorEnd
  deriving (Eq, Show, Generic)

-- | Correspond to the possible values of the
-- the attributes which are either `none` or
-- `url(#elem)`
data ElementRef
  = -- | Value for `none`
    RefNone
  | -- | Equivalent to `url()` attribute.
    Ref String
  deriving (Eq, Show, Generic)

data FilterSource
  = SourceGraphic
  | SourceAlpha
  | BackgroundImage
  | BackgroundAlpha
  | FillPaint
  | StrokePaint
  | SourceRef String
  deriving (Eq, Show, Generic)

data FilterAttributes = FilterAttributes
  { _filterHeight :: !(Maybe Number),
    _filterResult :: !(Maybe String),
    _filterWidth :: !(Maybe Number),
    _filterX :: !(Maybe Number),
    _filterY :: !(Maybe Number)
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg FilterAttributes where
  defaultSvg =
    FilterAttributes
      { _filterHeight = Nothing,
        _filterResult = Nothing,
        _filterWidth = Nothing,
        _filterX = Nothing,
        _filterY = Nothing
      }

-- | This type define how to draw any primitives,
-- which color to use, how to stroke the primitives
-- and the potential transformations to use.
--
-- All these attributes are propagated to the children.
data DrawAttributes = DrawAttributes
  { -- | Attribute corresponding to the `stroke-width`
    -- SVG attribute.
    _strokeWidth :: !(Maybe Number),
    -- | Correspond to the `stroke` attribute.
    _strokeColor :: !(Maybe Texture),
    -- | Define the `stroke-opacity` attribute, the transparency
    -- for the "border".
    _strokeOpacity :: !(Maybe Float),
    -- | Correspond to the `stroke-linecap` SVG
    -- attribute
    _strokeLineCap :: !(Maybe Cap),
    -- | Correspond to the `stroke-linejoin` SVG
    -- attribute
    _strokeLineJoin :: !(Maybe LineJoin),
    -- | Define the distance of the miter join, correspond
    -- to the `stroke-miterlimit` attritbue.
    _strokeMiterLimit :: !(Maybe Double),
    -- | Define the filling color of the elements. Corresponding
    -- to the `fill` attribute.
    _fillColor :: !(Maybe Texture),
    -- | Define the `fill-opacity` attribute, the transparency
    -- for the "content".
    _fillOpacity :: !(Maybe Float),
    -- | Define the global or group opacity attribute.
    _groupOpacity :: !(Maybe Float),
    -- | Content of the `transform` attribute
    _transform :: !(Maybe [Transformation]),
    -- | Define the `fill-rule` used during the rendering.
    _fillRule :: !(Maybe FillRule),
    -- | Define the `mask` attribute.
    _maskRef :: !(Maybe ElementRef),
    -- | Define the `clip-path` attribute.
    _clipPathRef :: !(Maybe ElementRef),
    -- | Define the `clip-rule` attribute.
    _clipRule :: !(Maybe FillRule),
    -- | Map to the `class` attribute. Used for the CSS
    -- rewriting.
    _attrClass :: ![T.Text],
    -- | Map to the `id` attribute. Used for the CSS
    -- rewriting.
    _attrId :: !(Maybe String),
    -- | Define the start distance of the dashing pattern.
    -- Correspond to the `stroke-dashoffset` attribute.
    _strokeOffset :: !(Maybe Number),
    -- | Define the dashing pattern for the lines. Correspond
    -- to the `stroke-dasharray` attribute.
    _strokeDashArray :: !(Maybe [Number]),
    -- | Current size of the text, correspond to the
    -- `font-size` SVG attribute.
    _fontSize :: !(Maybe Number),
    -- | Define the possible fonts to be used for text rendering.
    -- Map to the `font-family` attribute.
    _fontFamily :: !(Maybe [String]),
    -- | Map to the `font-style` attribute.
    _fontStyle :: !(Maybe FontStyle),
    -- | Define how to interpret the text position, correspond
    -- to the `text-anchor` attribute.
    _textAnchor :: !(Maybe TextAnchor),
    -- | Define the marker used for the start of the line.
    -- Correspond to the `marker-start` attribute.
    _markerStart :: !(Maybe ElementRef),
    -- | Define the marker used for every point of the
    -- polyline/path Correspond to the `marker-mid`
    -- attribute.
    _markerMid :: !(Maybe ElementRef),
    -- | Define the marker used for the end of the line.
    -- Correspond to the `marker-end` attribute.
    _markerEnd :: !(Maybe ElementRef),
    _filterRef :: !(Maybe ElementRef)
  }
  deriving (Eq, Show, Generic)

makeClassy ''DrawAttributes

-- | This primitive describe an unclosed suite of
-- segments. Correspond to the `<polyline>` tag.
data PolyLine = PolyLine
  { _polyLineDrawAttributes :: DrawAttributes,
    -- | Geometry definition of the polyline.
    -- correspond to the `points` attribute
    _polyLinePoints :: [RPoint]
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg PolyLine where
  defaultSvg = PolyLine mempty mempty

-- | Primitive decriving polygon composed
-- of segements. Correspond to the `<polygon>`
-- tag
data Polygon = Polygon
  { _polygonDrawAttributes :: DrawAttributes,
    -- | Points of the polygon. Correspond to
    -- the `points` attributes.
    _polygonPoints :: [RPoint]
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg Polygon where
  defaultSvg = Polygon mempty mempty

-- | Define a simple line. Correspond to the
-- `<line>` tag.
data Line = Line
  { _lineDrawAttributes :: DrawAttributes,
    -- | First point of the the line, correspond
    -- to the `x1` and `y1` attributes.
    _linePoint1 :: !Point,
    -- | Second point of the the line, correspond
    -- to the `x2` and `y2` attributes.
    _linePoint2 :: !Point
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg Line where
  defaultSvg =
    Line
      { _lineDrawAttributes = mempty,
        _linePoint1 = zeroPoint,
        _linePoint2 = zeroPoint
      }
    where
      zeroPoint = (Num 0, Num 0)

-- | Define a rectangle. Correspond to
-- `<rectangle>` svg tag.
data Rectangle = Rectangle
  { _rectangleDrawAttributes :: DrawAttributes,
    -- | Upper left corner of the rectangle, correspond
    -- to the attributes `x` and `y`.
    _rectUpperLeftCorner :: !Point,
    -- | Rectangle width, correspond, strangely, to
    -- the `width` attribute.
    _rectWidth :: !(Maybe Number),
    -- | Rectangle height, correspond, amazingly, to
    -- the `height` attribute.
    _rectHeight :: !(Maybe Number),
    -- | Define the rounded corner radius radius
    -- of the rectangle. Correspond to the `rx` and
    -- `ry` attributes.
    _rectCornerRadius :: !(Maybe Number, Maybe Number)
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg Rectangle where
  defaultSvg =
    Rectangle
      { _rectangleDrawAttributes = mempty,
        _rectUpperLeftCorner = (Num 0, Num 0),
        _rectWidth = Nothing,
        _rectHeight = Nothing,
        _rectCornerRadius = (Nothing, Nothing)
      }

-- | Type mapping the `<path>` svg tag.
data Path = Path
  { _pathDrawAttributes :: DrawAttributes,
    -- | Definition of the path, correspond to the
    -- `d` attributes.
    _pathDefinition :: [PathCommand]
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg Path where
  defaultSvg = Path mempty mempty

-- | Define a SVG group, corresponding `<g>` tag.
data Group = Group
  { _groupDrawAttributes :: DrawAttributes,
    -- | Content of the group, corresponding to all the tags
    -- inside the `<g>` tag.
    _groupChildren :: ![Tree],
    -- | Mapped to the attribute `viewBox`
    _groupViewBox :: !(Maybe (Double, Double, Double, Double)),
    -- | used for symbols only
    _groupAspectRatio :: !PreserveAspectRatio
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg Group where
  defaultSvg =
    Group
      { _groupDrawAttributes = mempty,
        _groupChildren = [],
        _groupViewBox = Nothing,
        _groupAspectRatio = defaultSvg
      }

-- | Define the `<filter>` tag.
data Filter = Filter
  { _filterDrawAttributes :: DrawAttributes,
    _filterSelfAttributes :: !FilterAttributes,
    _filterChildren :: ![FilterElement]
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg Filter where
  defaultSvg =
    Filter
      { _filterDrawAttributes = mempty,
        _filterSelfAttributes = defaultSvg,
        _filterChildren = []
      }

-- | Define a `<circle>`.
data Circle = Circle
  { _circleDrawAttributes :: DrawAttributes,
    -- | Define the center of the circle, describe
    -- the `cx` and `cy` attributes.
    _circleCenter :: !Point,
    -- | Radius of the circle, equivalent to the `r`
    -- attribute.
    _circleRadius :: !Number
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg Circle where
  defaultSvg =
    Circle
      { _circleDrawAttributes = mempty,
        _circleCenter = (Num 0, Num 0),
        _circleRadius = Num 0
      }

-- | Define an `<ellipse>`
data Ellipse = Ellipse
  { _ellipseDrawAttributes :: DrawAttributes,
    -- | Center of the ellipse, map to the `cx`
    -- and `cy` attributes.
    _ellipseCenter :: !Point,
    -- | Radius along the X axis, map the
    -- `rx` attribute.
    _ellipseXRadius :: !Number,
    -- | Radius along the Y axis, map the
    -- `ry` attribute.
    _ellipseYRadius :: !Number
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg Ellipse where
  defaultSvg =
    Ellipse
      { _ellipseDrawAttributes = mempty,
        _ellipseCenter = (Num 0, Num 0),
        _ellipseXRadius = Num 0,
        _ellipseYRadius = Num 0
      }

-- | Define a color stop for the gradients. Represent
-- the `<stop>` SVG tag.
data GradientStop = GradientStop
  { -- | Gradient offset between 0 and 1, correspond
    -- to the `offset` attribute.
    _gradientOffset :: !Float,
    -- | Color of the gradient stop. Correspond
    -- to the `stop-color` attribute.
    _gradientColor :: !PixelRGBA8,
    -- | Path command used in mesh patch
    _gradientPath :: !(Maybe GradientPathCommand),
    -- | Stop color opacity
    _gradientOpacity :: !(Maybe Float)
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg GradientStop where
  defaultSvg =
    GradientStop
      { _gradientOffset = 0.0,
        _gradientColor = PixelRGBA8 0 0 0 255,
        _gradientPath = Nothing,
        _gradientOpacity = Nothing
      }

-- | Define `<meshpatch>` SVG tag
data MeshGradientPatch = MeshGradientPatch
  { -- | List of stop, from 2 to 4 in a patch
    _meshGradientPatchStops :: ![GradientStop]
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg MeshGradientPatch where
  defaultSvg = MeshGradientPatch []

-- | Define a `<meshrow>` tag.
data MeshGradientRow = MeshGradientRow
  { -- | List of patch in a row
    _meshGradientRowPatches :: ![MeshGradientPatch]
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg MeshGradientRow where
  defaultSvg = MeshGradientRow []

-- | Define a `<meshgradient>` tag.
data MeshGradient = MeshGradient
  { _meshGradientDrawAttributes :: DrawAttributes,
    -- | Original x coordinate of the mesh gradient
    _meshGradientX :: !Number,
    -- | Original y coordinate of the mesh gradient
    _meshGradientY :: !Number,
    -- | Type of color interpolation to use
    _meshGradientType :: !MeshGradientType,
    -- | Coordiante system to use
    _meshGradientUnits :: !CoordinateUnits,
    -- | Optional transform
    _meshGradientTransform :: ![Transformation],
    -- | List of patch rows in the the mesh.
    _meshGradientRows :: ![MeshGradientRow]
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg MeshGradient where
  defaultSvg =
    MeshGradient
      { _meshGradientDrawAttributes = mempty,
        _meshGradientX = Percent 0,
        _meshGradientY = Percent 0,
        _meshGradientType = GradientBilinear,
        _meshGradientUnits = CoordBoundingBox,
        _meshGradientTransform = mempty,
        _meshGradientRows = mempty
      }

-- | Define an `<image>` tag.
data Image = Image
  { _imageDrawAttributes :: DrawAttributes,
    -- | Position of the image referenced by its
    -- upper left corner.
    _imageCornerUpperLeft :: !Point,
    -- | Image width
    _imageWidth :: !Number,
    -- | Image Height
    _imageHeight :: !Number,
    -- | Image href, pointing to the real image.
    _imageHref :: !String,
    -- | preserveAspectRatio attribute
    _imageAspectRatio :: !PreserveAspectRatio
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg Image where
  defaultSvg =
    Image
      { _imageDrawAttributes = mempty,
        _imageCornerUpperLeft = (Num 0, Num 0),
        _imageWidth = Num 0,
        _imageHeight = Num 0,
        _imageHref = "",
        _imageAspectRatio = defaultSvg
      }

-- | Define an `<use>` for a named content.
-- Every named content can be reused in the
-- document using this element.
data Use = Use
  { _useDrawAttributes :: DrawAttributes,
    -- | Position where to draw the "used" element.
    -- Correspond to the `x` and `y` attributes.
    _useBase :: Point,
    -- | Referenced name, correspond to `xlink:href`
    -- attribute.
    _useName :: String,
    -- | Define the width of the region where
    -- to place the element. Map to the `width`
    -- attribute.
    _useWidth :: Maybe Number,
    -- | Define the height of the region where
    -- to place the element. Map to the `height`
    -- attribute.
    _useHeight :: Maybe Number
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg Use where
  defaultSvg =
    Use
      { _useDrawAttributes = mempty,
        _useBase = (Num 0, Num 0),
        _useName = "",
        _useWidth = Nothing,
        _useHeight = Nothing
      }

-- | Define position information associated to
-- `<text>` or `<tspan>` svg tag.
data TextInfo = TextInfo
  { -- | `x` attribute.
    _textInfoX :: ![Number],
    -- | `y` attribute.
    _textInfoY :: ![Number],
    -- | `dx` attribute.
    _textInfoDX :: ![Number],
    -- | `dy` attribute.
    _textInfoDY :: ![Number],
    -- | `rotate` attribute.
    _textInfoRotate :: ![Double],
    -- | `textLength` attribute.
    _textInfoLength :: !(Maybe Number)
  }
  deriving (Eq, Show, Generic)

instance Semigroup TextInfo where
  (<>)
    (TextInfo x1 y1 dx1 dy1 r1 l1)
    (TextInfo x2 y2 dx2 dy2 r2 l2) =
      TextInfo
        (x1 <> x2)
        (y1 <> y2)
        (dx1 <> dx2)
        (dy1 <> dy2)
        (r1 <> r2)
        (l2 <|> l1)

instance Monoid TextInfo where
  mempty = TextInfo [] [] [] [] [] Nothing
  mappend = (<>)

instance WithDefaultSvg TextInfo where
  defaultSvg = mempty

-- | Define the content of a `<tspan>` tag.
data TextSpanContent
  = -- | Raw text
    SpanText !T.Text
  | -- | Equivalent to a `<tref>`
    SpanTextRef !String
  | -- | Define a `<tspan>`
    SpanSub !TextSpan
  deriving (Eq, Show, Generic)

-- | Define a `<tspan>` tag.
data TextSpan = TextSpan
  { -- | Placing information for the text.
    _spanInfo :: !TextInfo,
    -- | Drawing attributes for the text span.
    _spanDrawAttributes :: !DrawAttributes,
    -- | Content of the span.
    _spanContent :: ![TextSpanContent]
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg TextSpan where
  defaultSvg =
    TextSpan
      { _spanInfo = defaultSvg,
        _spanDrawAttributes = mempty,
        _spanContent = mempty
      }

-- | Describe the content of the `method` attribute on
-- text path.
data TextPathMethod
  = -- | Map to the `align` value.
    TextPathAlign
  | -- | Map to the `stretch` value.
    TextPathStretch
  deriving (Eq, Show, Generic)

-- | Describe the content of the `spacing` text path
-- attribute.
data TextPathSpacing
  = -- | Map to the `exact` value.
    TextPathSpacingExact
  | -- | Map to the `auto` value.
    TextPathSpacingAuto
  deriving (Eq, Show, Generic)

-- | Describe the `<textpath>` SVG tag.
data TextPath = TextPath
  { -- | Define the beginning offset on the path,
    -- the `startOffset` attribute.
    _textPathStartOffset :: !Number,
    -- | Define the `xlink:href` attribute.
    _textPathName :: !String,
    -- | Correspond to the `method` attribute.
    _textPathMethod :: !TextPathMethod,
    -- | Correspond to the `spacing` attribute.
    _textPathSpacing :: !TextPathSpacing
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg TextPath where
  defaultSvg =
    TextPath
      { _textPathStartOffset = Num 0,
        _textPathName = mempty,
        _textPathMethod = TextPathAlign,
        _textPathSpacing = TextPathSpacingExact
      }

-- | Define the possible values of the `lengthAdjust`
-- attribute.
data TextAdjust
  = -- | Value `spacing`
    TextAdjustSpacing
  | -- | Value `spacingAndGlyphs`
    TextAdjustSpacingAndGlyphs
  deriving (Eq, Show, Generic)

-- | Define the global `<text>` SVG tag.
data Text = Text
  { -- | Define the `lengthAdjust` attribute.
    _textAdjust :: !TextAdjust,
    -- | Root of the text content.
    _textRoot :: !TextSpan
  }
  deriving (Eq, Show, Generic)

-- | Little helper to create a SVG text at a given
-- baseline position.
textAt :: Point -> T.Text -> Text
textAt (x, y) txt = Text TextAdjustSpacing tspan
  where
    tspan =
      defaultSvg
        { _spanContent = [SpanText txt],
          _spanInfo =
            defaultSvg
              { _textInfoX = [x],
                _textInfoY = [y]
              }
        }

instance WithDefaultSvg Text where
  defaultSvg =
    Text
      { _textRoot = defaultSvg,
        _textAdjust = TextAdjustSpacing
      }

-- | Main type for the scene description, reorient to
-- specific type describing each tag.
data Tree = CachedTree
  { _treeBranch :: TreeBranch,
    _treeHash :: Int
  }
  deriving (Eq, Show, Generic)

data TreeBranch
  = NoNode
  | UseNode
      { useInformation :: !Use,
        useSubTree :: !(Maybe Tree)
      }
  | GroupNode !Group
  | SymbolNode !Group
  | DefinitionNode !Group
  | FilterNode !Filter
  | PathNode !Path
  | CircleNode !Circle
  | PolyLineNode !PolyLine
  | PolygonNode !Polygon
  | EllipseNode !Ellipse
  | LineNode !Line
  | RectangleNode !Rectangle
  | TextNode !(Maybe TextPath) !Text
  | ImageNode !Image
  | LinearGradientNode !LinearGradient
  | RadialGradientNode !RadialGradient
  | MeshGradientNode !MeshGradient
  | PatternNode !Pattern
  | MarkerNode !Marker
  | MaskNode !Mask
  | ClipPathNode !ClipPath
  | SvgNode !Document
  deriving (Eq, Show, Generic)

instance WithDefaultSvg TreeBranch where
  defaultSvg = NoNode

data FilterElement
  = FEBlend Blend               -- SVG Basic --DONE
  | FEColorMatrix ColorMatrix   -- SVG Basic --DONE
  | FEComponentTransfer ComponentTransfer -- Need -- SVG Basic --DONE --Parser not working
  | FEComposite Composite       -- SVG Basic --DONE
  | FEConvolveMatrix ConvolveMatrix          --DONE -- No parser
  | FEDiffuseLighting DiffuseLighting        --DONE -- No parser
  | FEDisplacementMap DisplacementMap        --DONE
  | FEDropShadow DropShadow                  --DONE -- No parser
  | FEFlood Flood               -- SVG Basic --DONE
  | FEFuncA FuncA -- Need       -- SVG Basic --DONE
  | FEFuncB FuncB               -- SVG Basic --DONE
  | FEFuncG FuncG               -- SVG Basic --DONE
  | FEFuncR FuncR               -- SVG Basic --DONE
  | FEGaussianBlur GaussianBlur -- SVG Basic --DONE
  | FEImage ImageF              -- SVG Basic --DONE --Parser not working
  | FEMerge Merge               -- SVG Basic --DONE
  | FEMergeNode MergeNode       -- SVG Basic --DONE
  | FEMorphology Morphology                  --DONE -- No parser
  | FEOffset Offset             -- SVG Basic --DONE
  | FESpecularLighting SpecularLighting      --DONE -- No parser
  | FETile Tile                 -- SVG Basic --DONE
  | FETurbulence Turbulence                  --DONE
  | FENone
  deriving (Eq, Show, Generic)

instance WithDefaultSvg FilterElement where
  defaultSvg = FENone

data SpecularLighting = SpecularLighting
  { _specLightingDrawAttributes :: DrawAttributes,
    _specLightingFilterAttr :: !FilterAttributes,
    _specLightingIn :: !(Maybe FilterSource),
    _specLightingSurfaceScale :: Double,
    _specLightingSpecularConst :: Double,
    _specLightingSpecularExp :: Double,
    _specLightingKernelUnitLength :: NumberOptionalNumber
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg SpecularLighting where
  defaultSvg =
    SpecularLighting
    { _specLightingDrawAttributes = defaultSvg,
      _specLightingFilterAttr = defaultSvg,
      _specLightingIn = Nothing,
      _specLightingSurfaceScale = 1,
      _specLightingSpecularConst = 1,
      _specLightingSpecularExp = 1,
      _specLightingKernelUnitLength = Num1 0
    }

data ConvolveMatrix = ConvolveMatrix
  { _convolveMatrixDrawAttributes :: DrawAttributes,
    _convolveMatrixFilterAttr :: !FilterAttributes,
    _convolveMatrixIn :: !(Maybe FilterSource),
    _convolveMatrixOrder :: NumberOptionalNumber,
    _convolveMatrixKernelMatrix :: [Double],
    _convolveMatrixDivisor :: Double,
    _convolveMatrixBias :: Double,
    _convolveMatrixTargetX :: Int,
    _convolveMatrixTargetY :: Int,
    _convolveMatrixEdgeMode :: EdgeMode,
    _convolveMatrixKernelUnitLength :: NumberOptionalNumber,
    _convolveMatrixPreserveAlpha :: Bool
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg ConvolveMatrix where
  defaultSvg =
    ConvolveMatrix
    { _convolveMatrixDrawAttributes = defaultSvg,
      _convolveMatrixFilterAttr = defaultSvg,
      _convolveMatrixIn = Nothing,
      _convolveMatrixOrder = Num1 3,
      _convolveMatrixKernelMatrix = [],
      _convolveMatrixDivisor = 1,
      _convolveMatrixBias = 0,
      _convolveMatrixTargetX = 1,
      _convolveMatrixTargetY = 1,
      _convolveMatrixEdgeMode = EdgeDuplicate,
      _convolveMatrixKernelUnitLength = Num1 0,
      _convolveMatrixPreserveAlpha = False
    }

data DiffuseLighting = DiffuseLighting
  { _diffuseLightingDrawAttributes :: DrawAttributes,
    _diffuseLightingFilterAttr :: !FilterAttributes,
    _diffuseLightingIn :: !(Maybe FilterSource),
    _diffuseLightingSurfaceScale :: Double,
    _diffuseLightingDiffuseConst :: Double,
    _diffuseLightingKernelUnitLength :: NumberOptionalNumber
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg DiffuseLighting where
  defaultSvg =
    DiffuseLighting
    { _diffuseLightingDrawAttributes = defaultSvg,
      _diffuseLightingFilterAttr = defaultSvg,
      _diffuseLightingIn = Nothing,
      _diffuseLightingSurfaceScale = 1,
      _diffuseLightingDiffuseConst = 1,
      _diffuseLightingKernelUnitLength = Num1 0
    }

data Morphology = Morphology
  { _morphologyDrawAttributes :: DrawAttributes,
    _morphologyFilterAttr :: !FilterAttributes,
    _morphologyIn :: !(Maybe FilterSource),
    _morphologyOperator :: OperatorType,
    _morphologyRadius :: NumberOptionalNumber
  }
  deriving (Eq, Show, Generic)


instance WithDefaultSvg Morphology where
  defaultSvg =
    Morphology
    { _morphologyDrawAttributes = defaultSvg,
      _morphologyFilterAttr = defaultSvg,
      _morphologyIn = Nothing,
      _morphologyOperator = OperatorOver,
      _morphologyRadius = Num1 0
    }

data DropShadow = DropShadow
  { _dropShadowDrawAttributes :: DrawAttributes,
    _dropShadowFilterAttr :: !FilterAttributes,
    _dropShadowDx :: Double,
    _dropShadowDy :: Double,
    _dropShadowStdDeviation :: NumberOptionalNumber
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg DropShadow where
  defaultSvg =
    DropShadow
    { _dropShadowDrawAttributes = defaultSvg,
      _dropShadowFilterAttr = defaultSvg,
      _dropShadowDx = 2,
      _dropShadowDy = 2,
      _dropShadowStdDeviation = Num1 0
    }

data OperatorType
  = OperatorOver
  | OperatorIn
  | OperatorOut
  | OperatorAtop
  | OperatorXor
  | OperatorLighter
  | OperatorArithmetic
  deriving (Eq, Show, Generic)

data NumberOptionalNumber
  = Num1 Double
  | Num2 Double Double
  deriving (Eq, Show, Generic)

data ImageF = ImageF
  { _imageFDrawAttributes :: DrawAttributes,
    _imageFFilterAttr :: !FilterAttributes,
    _imageFHref :: !String,
    _imageFAspectRatio :: !PreserveAspectRatio
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg ImageF where
  defaultSvg =
    ImageF
      { _imageFDrawAttributes = defaultSvg,
        _imageFFilterAttr = defaultSvg,
        _imageFHref = "",
        _imageFAspectRatio = defaultSvg
      }

data TransferFunctionType
  = TFIdentity
  | TFTable
  | TFDiscrete
  | TFLinear
  | TFGamma
  deriving (Eq, Show, Generic)

data TransferFunction = TransferFunction
  { _transferFunctionDrawAttributes :: !DrawAttributes,
    _transferFunctionFilterAttr :: !FilterAttributes,
    _transferFunctionType :: TransferFunctionType,
    _transferFunctionTableValues :: [Double],
    _transferFunctionSlope :: Double,
    _transferFunctionIntercept :: Double,
    _transferFunctionAmplitude :: Double,
    _transferFunctionExponent :: Double,
    _transferFunctionOffset :: Double
  }
  deriving (Eq, Show, Generic)

data ChannelSelector
  = ChannelR
  | ChannelG
  | ChannelB
  | ChannelA
  deriving (Eq, Show, Generic)

data DisplacementMap = DisplacementMap
  { _displacementMapDrawAttributes :: !DrawAttributes,
    _displacementMapFilterAttr :: !FilterAttributes,
    _displacementMapIn :: !(Maybe FilterSource),
    _displacementMapIn2 :: !(Maybe FilterSource),
    _displacementMapScale :: !(Maybe Double),
    _displacementMapXChannelSelector :: ChannelSelector,
    _displacementMapYChannelSelector :: ChannelSelector
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg DisplacementMap where
  defaultSvg =
    DisplacementMap
      { _displacementMapDrawAttributes = defaultSvg,
        _displacementMapFilterAttr = defaultSvg,
        _displacementMapIn = Nothing,
        _displacementMapIn2 = Nothing,
        _displacementMapScale = Nothing,
        _displacementMapXChannelSelector = ChannelA,
        _displacementMapYChannelSelector = ChannelA
      }

data BlendMode
  = Normal
  | Multiply
  | Screen
  | Overlay
  | Darken
  | Lighten
  | ColorDodge
  | ColorBurn
  | HardLight
  | SoftLight
  | Difference
  | Exclusion
  | Hue
  | Saturation
  | Color
  | Luminosity
  deriving (Eq, Show, Generic)

data Blend = Blend
  { _blendDrawAttributes :: !DrawAttributes,
    _blendFilterAttr :: !FilterAttributes,
    _blendIn :: !(Maybe FilterSource),
    _blendIn2 :: !(Maybe FilterSource),
    _blendMode :: !BlendMode
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg Blend where
  defaultSvg =
    Blend
    { _blendDrawAttributes = defaultSvg,
      _blendFilterAttr = defaultSvg,
      _blendIn = Nothing,
      _blendIn2 = Nothing,
      _blendMode = Normal
    }

data Flood = Flood
  { _floodDrawAttributes :: !DrawAttributes,
    _floodFilterAttr :: !FilterAttributes,
    _floodColor :: !PixelRGBA8,
    _floodOpacity :: !(Maybe Double)
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg Flood where
  defaultSvg =
    Flood
    { _floodDrawAttributes = defaultSvg,
      _floodFilterAttr = defaultSvg,
      _floodColor = PixelRGBA8 0 0 0 255,
      _floodOpacity = Just 1.0
    }

data Offset = Offset
  { _offsetDrawAttributes :: !DrawAttributes,
    _offsetFilterAttr :: !FilterAttributes,
    _offsetIn :: !(Maybe FilterSource),
    _offsetDX :: !Number,
    _offsetDY :: !Number
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg Offset where
  defaultSvg =
    Offset
    { _offsetDrawAttributes = defaultSvg,
      _offsetFilterAttr = defaultSvg,
      _offsetIn = Nothing,
      _offsetDX = Num 0,
      _offsetDY = Num 0
    }

data Tile = Tile
  { _tileDrawAttributes :: !DrawAttributes,
    _tileFilterAttr :: !FilterAttributes,
    _tileIn :: !(Maybe FilterSource)
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg Tile where
  defaultSvg =
    Tile
    { _tileDrawAttributes = defaultSvg,
      _tileFilterAttr = defaultSvg,
      _tileIn = Nothing
    }

data Merge = Merge
  { _mergeDrawAttributes :: !DrawAttributes,
    _mergeFilterAttributes :: !FilterAttributes,
    _mergeChildren :: ![FilterElement]
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg Merge where
  defaultSvg =
    Merge
    { _mergeDrawAttributes = defaultSvg,
      _mergeFilterAttributes = defaultSvg,
      _mergeChildren = []
    }

data MergeNode = MergeNode
  { _mergeNodeDrawAttributes :: !DrawAttributes,
    --Does not have filter attributes!
    _mergeNodeIn :: !(Maybe FilterSource)
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg MergeNode where
  defaultSvg =
    MergeNode
    { _mergeNodeDrawAttributes = defaultSvg,
      _mergeNodeIn = Nothing
    }

data ComponentTransfer = ComponentTransfer
  { _compTransferDrawAttributes :: !DrawAttributes,
    _compTransferFilterAttr :: !FilterAttributes,
    _compTransferChildren :: ![FilterElement],
    _compTransferIn :: !(Maybe FilterSource)
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg ComponentTransfer where
  defaultSvg =
    ComponentTransfer
    { _compTransferDrawAttributes = defaultSvg,
      _compTransferFilterAttr = defaultSvg,
      _compTransferChildren = [],
      _compTransferIn = Nothing
    }

data FuncType
  = FIdentity
  | FTable
  | FDiscrete
  | FLinear
  | FGamma
  deriving (Eq, Show, Generic)

data FuncA = FuncA
  { _funcADrawAttributes :: !DrawAttributes,
    --Does not have filter attributes!
    _funcAType :: !FuncType,
    _funcATableValues :: ![Number],
    _funcASlope :: !Number,
    _funcAIntercept :: !Number,
    _funcAAmplitude :: !Number,
    _funcAExponent :: !Number
    --_funcAOffset :: _ -- This appears in the documentation, but no details are given.
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg FuncA where
  defaultSvg =
    FuncA
    { _funcADrawAttributes = defaultSvg,
      _funcAType = FIdentity, -- Standard does not define a default value.
      _funcATableValues = [],
      _funcASlope = Num 0,
      _funcAIntercept = Num 0,
      _funcAAmplitude = Num 1,
      _funcAExponent = Num 1
    }

data FuncR = FuncR
  { _funcRDrawAttributes :: !DrawAttributes,
    --Does not have filter attributes!
    _funcRType :: !FuncType,
    _funcRTableValues :: ![Number],
    _funcRSlope :: !Number,
    _funcRIntercept :: !Number,
    _funcRAmplitude :: !Number,
    _funcRExponent :: !Number
    --_funcAOffset :: _ -- This appears in the documentation, but no details are given.
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg FuncR where
  defaultSvg =
    FuncR
    { _funcRDrawAttributes = defaultSvg,
      _funcRType = FIdentity, -- Standard does not define a default value.
      _funcRTableValues = [],
      _funcRSlope = Num 0,
      _funcRIntercept = Num 0,
      _funcRAmplitude = Num 1,
      _funcRExponent = Num 1
    }

data FuncG = FuncG
  { _funcGDrawAttributes :: !DrawAttributes,
    --Does not have filter attributes!
    _funcGType :: !FuncType,
    _funcGTableValues :: ![Number],
    _funcGSlope :: !Number,
    _funcGIntercept :: !Number,
    _funcGAmplitude :: !Number,
    _funcGExponent :: !Number
    --_funcAOffset :: _ -- This appears in the documentation, but no details are given.
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg FuncG where
  defaultSvg =
    FuncG
    { _funcGDrawAttributes = defaultSvg,
      _funcGType = FIdentity, -- Standard does not define a default value.
      _funcGTableValues = [],
      _funcGSlope = Num 0,
      _funcGIntercept = Num 0,
      _funcGAmplitude = Num 1,
      _funcGExponent = Num 1
    }

data FuncB = FuncB
  { _funcBDrawAttributes :: !DrawAttributes,
    --Does not have filter attributes!
    _funcBType :: !FuncType,
    _funcBTableValues :: ![Number],
    _funcBSlope :: !Number,
    _funcBIntercept :: !Number,
    _funcBAmplitude :: !Number,
    _funcBExponent :: !Number
    --_funcAOffset :: _ -- This appears in the documentation, but no details are given.
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg FuncB where
  defaultSvg =
    FuncB
    { _funcBDrawAttributes = defaultSvg,
      _funcBType = FIdentity, -- Standard does not define a default value.
      _funcBTableValues = [],
      _funcBSlope = Num 0,
      _funcBIntercept = Num 0,
      _funcBAmplitude = Num 1,
      _funcBExponent = Num 1
    }


data ColorMatrixType
  = Matrix
  | Saturate
  | HueRotate
  | LuminanceToAlpha
  deriving (Eq, Show, Generic)

data ColorMatrix = ColorMatrix
  { _colorMatrixDrawAttributes :: !DrawAttributes,
    _colorMatrixFilterAttr :: !FilterAttributes,
    _colorMatrixIn :: !(Maybe FilterSource),
    _colorMatrixType :: !ColorMatrixType,
    _colorMatrixValues :: !String
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg ColorMatrix where
  defaultSvg =
    ColorMatrix
      { _colorMatrixDrawAttributes = defaultSvg,
        _colorMatrixFilterAttr = defaultSvg,
        _colorMatrixIn = Nothing,
        _colorMatrixType = Matrix,
        _colorMatrixValues = ""
      }

data CompositeOperator
  = CompositeOver -- this is default
  | CompositeIn
  | CompositeOut
  | CompositeAtop
  | CompositeXor
  | CompositeArithmetic
  deriving (Eq, Show, Generic)

data Composite = Composite
  { _compositeDrawAttributes :: DrawAttributes,
    _compositeFilterAttr :: !FilterAttributes,
    _compositeIn :: Maybe FilterSource,
    _compositeIn2 :: Maybe FilterSource,
    _compositeOperator :: CompositeOperator,
    _compositeK1 :: Number,
    _compositeK2 :: Number,
    _compositeK3 :: Number,
    _compositeK4 :: Number
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg Composite where
  defaultSvg =
    Composite
      { _compositeDrawAttributes = defaultSvg,
        _compositeFilterAttr = defaultSvg,
        _compositeIn = Nothing,
        _compositeIn2 = Nothing,
        _compositeOperator = CompositeOver,
        _compositeK1 = Num 0,
        _compositeK2 = Num 0,
        _compositeK3 = Num 0,
        _compositeK4 = Num 0
      }

data Turbulence = Turbulence
  { _turbulenceDrawAttributes :: !DrawAttributes,
    _turbulenceFilterAttr :: !FilterAttributes,
    _turbulenceBaseFrequency :: !(Double, Maybe Double), -- Not negative
    _turbulenceNumOctaves :: Int, -- Not negative
    _turbulenceSeed :: Double,
    _turbulenceStitchTiles :: StitchTiles,
    _turbulenceType :: TurbulenceType
  }
  deriving (Eq, Show, Generic)

data StitchTiles
  = NoStitch
  | Stitch
  deriving (Eq, Show, Generic)

data TurbulenceType
  = FractalNoiseType
  | TurbulenceType
  deriving (Eq, Show, Generic)

instance WithDefaultSvg Turbulence where
  defaultSvg =
    Turbulence
      { _turbulenceDrawAttributes = defaultSvg,
        _turbulenceFilterAttr = defaultSvg,
        _turbulenceBaseFrequency = (0, Nothing),
        _turbulenceNumOctaves = 1,
        _turbulenceSeed = 0,
        _turbulenceStitchTiles = NoStitch,
        _turbulenceType = TurbulenceType
      }

data EdgeMode
  = EdgeDuplicate
  | EdgeWrap
  | EdgeNone
  deriving (Eq, Show, Generic)

data GaussianBlur = GaussianBlur
  { _gaussianBlurDrawAttributes :: DrawAttributes,
    _gaussianBlurFilterAttr :: !FilterAttributes,
    _gaussianBlurIn :: Maybe FilterSource,
    _gaussianBlurStdDeviationX :: Number,
    _gaussianBlurStdDeviationY :: Maybe Number,
    _gaussianBlurEdgeMode :: EdgeMode
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg GaussianBlur where
  defaultSvg =
    GaussianBlur
      { _gaussianBlurDrawAttributes = defaultSvg,
        _gaussianBlurFilterAttr = defaultSvg,
        _gaussianBlurIn = Nothing,
        _gaussianBlurStdDeviationX = Num 0,
        _gaussianBlurStdDeviationY = Nothing,
        _gaussianBlurEdgeMode = EdgeDuplicate
      }

-- | Define the orientation, associated to the
-- `orient` attribute on the Marker
data MarkerOrientation
  = -- | Auto value
    OrientationAuto
  | -- | Specific angle.
    OrientationAngle Coord
  deriving (Eq, Show, Generic)

-- | Define the content of the `markerUnits` attribute
-- on the Marker.
data MarkerUnit
  = -- | Value `strokeWidth`
    MarkerUnitStrokeWidth
  | -- | Value `userSpaceOnUse`
    MarkerUnitUserSpaceOnUse
  deriving (Eq, Show, Generic)

-- | Define the content of the `markerUnits` attribute
-- on the Marker.
data Overflow
  = -- | Value `visible`
    OverflowVisible
  | -- | Value `hidden`
    OverflowHidden
  deriving (Eq, Show, Generic)

-- | Define the `<marker>` tag.
data Marker = Marker
  { _markerDrawAttributes :: DrawAttributes,
    -- | Define the reference point of the marker.
    -- correspond to the `refX` and `refY` attributes.
    _markerRefPoint :: !(Number, Number),
    -- | Define the width of the marker. Correspond to
    -- the `markerWidth` attribute.
    _markerWidth :: !(Maybe Number),
    -- | Define the height of the marker. Correspond to
    -- the `markerHeight` attribute.
    _markerHeight :: !(Maybe Number),
    -- | Correspond to the `orient` attribute.
    _markerOrient :: !(Maybe MarkerOrientation),
    -- | Map the `markerUnits` attribute.
    _markerUnits :: !(Maybe MarkerUnit),
    -- | Optional viewbox
    _markerViewBox :: !(Maybe (Double, Double, Double, Double)),
    -- | Elements defining the marker.
    _markerOverflow :: !(Maybe Overflow),
    -- | preserveAspectRatio attribute
    _markerAspectRatio :: !PreserveAspectRatio,
    -- | Elements defining the marker.
    _markerElements :: [Tree]
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg Marker where
  defaultSvg =
    Marker
      { _markerDrawAttributes = mempty,
        _markerRefPoint = (Num 0, Num 0),
        _markerWidth = Just (Num 3),
        _markerHeight = Just (Num 3),
        _markerOrient = Nothing, -- MarkerOrientation
        _markerUnits = Nothing, -- MarkerUnitStrokeWidth
        _markerViewBox = Nothing,
        _markerOverflow = Nothing,
        _markerElements = mempty,
        _markerAspectRatio = defaultSvg
      }

-- | For every element of a svg tree, associate
-- it's SVG tag name.
nameOfTree :: Tree -> T.Text
nameOfTree v =
  case _treeBranch v of
    NoNode -> ""
    UseNode _ _ -> "use"
    GroupNode _ -> "g"
    SymbolNode _ -> "symbol"
    DefinitionNode _ -> "defs"
    FilterNode _ -> "filter"
    PathNode _ -> "path"
    CircleNode _ -> "circle"
    PolyLineNode _ -> "polyline"
    PolygonNode _ -> "polygon"
    EllipseNode _ -> "ellipse"
    LineNode _ -> "line"
    RectangleNode _ -> "rectangle"
    TextNode _ _ -> "text"
    ImageNode _ -> "image"
    LinearGradientNode _ -> "lineargradient"
    RadialGradientNode _ -> "radialgradient"
    MeshGradientNode _ -> "meshgradient"
    PatternNode _ -> "pattern"
    MarkerNode _ -> "marker"
    MaskNode _ -> "mask"
    ClipPathNode _ -> "clipPath"
    SvgNode {} -> "svg"

-- | Define the possible values for the `spreadMethod`
-- values used for the gradient definitions.
data Spread
  = -- | `reapeat` value
    SpreadRepeat
  | -- | `pad` value
    SpreadPad
  | -- | `reflect value`
    SpreadReflect
  deriving (Eq, Show, Generic)

-- | Define a `<linearGradient>` tag.
data LinearGradient = LinearGradient
  { _linearGradientDrawAttributes :: DrawAttributes,
    -- | Define coordinate system of the gradient,
    -- associated to the `gradientUnits` attribute.
    _linearGradientUnits :: CoordinateUnits,
    -- | Point defining the beginning of the line gradient.
    -- Associated to the `x1` and `y1` attribute.
    _linearGradientStart :: Point,
    -- | Point defining the end of the line gradient.
    -- Associated to the `x2` and `y2` attribute.
    _linearGradientStop :: Point,
    -- | Define how to handle the values outside
    -- the gradient start and stop. Associated to the
    -- `spreadMethod` attribute.
    _linearGradientSpread :: Spread,
    -- | Define the transformation to apply to the
    -- gradient points. Associated to the `gradientTransform`
    -- attribute.
    _linearGradientTransform :: [Transformation],
    -- | List of color stops of the linear gradient.
    _linearGradientStops :: [GradientStop]
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg LinearGradient where
  defaultSvg =
    LinearGradient
      { _linearGradientDrawAttributes = mempty,
        _linearGradientUnits = CoordBoundingBox,
        _linearGradientStart = (Percent 0, Percent 0),
        _linearGradientStop = (Percent 1, Percent 0),
        _linearGradientSpread = SpreadPad,
        _linearGradientTransform = [],
        _linearGradientStops = []
      }

-- | Define a `<radialGradient>` tag.
data RadialGradient = RadialGradient
  { _radialGradientDrawAttributes :: DrawAttributes,
    -- | Define coordinate system of the gradient,
    -- associated to the `gradientUnits` attribute.
    _radialGradientUnits :: CoordinateUnits,
    -- | Center of the radial gradient. Associated to
    -- the `cx` and `cy` attributes.
    _radialGradientCenter :: Point,
    -- | Radius of the radial gradient. Associated to
    -- the `r` attribute.
    _radialGradientRadius :: Number,
    -- | X coordinate of the focus point of the radial
    -- gradient. Associated to the `fx` attribute.
    _radialGradientFocusX :: Maybe Number,
    -- | Y coordinate of the focus point of the radial
    -- gradient. Associated to the `fy` attribute.
    _radialGradientFocusY :: Maybe Number,
    -- | Define how to handle the values outside
    -- the gradient start and stop. Associated to the
    -- `spreadMethod` attribute.
    _radialGradientSpread :: Spread,
    -- | Define the transformation to apply to the
    -- gradient points. Associated to the `gradientTransform`
    -- attribute.
    _radialGradientTransform :: [Transformation],
    -- | List of color stops of the radial gradient.
    _radialGradientStops :: [GradientStop]
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg RadialGradient where
  defaultSvg =
    RadialGradient
      { _radialGradientDrawAttributes = mempty,
        _radialGradientUnits = CoordBoundingBox,
        _radialGradientCenter = (Percent 0.5, Percent 0.5),
        _radialGradientRadius = Percent 0.5,
        _radialGradientFocusX = Nothing,
        _radialGradientFocusY = Nothing,
        _radialGradientSpread = SpreadPad,
        _radialGradientTransform = [],
        _radialGradientStops = []
      }

-- | Define a SVG `<mask>` tag.
data Mask = Mask
  { _maskDrawAttributes :: DrawAttributes,
    -- | Correspond to the `maskContentUnits` attributes.
    _maskContentUnits :: CoordinateUnits,
    -- | Mapping to the `maskUnits` attribute.
    _maskUnits :: CoordinateUnits,
    -- | Map to the `x` and `y` attributes.
    _maskPosition :: Point,
    -- | Map to the `width` attribute
    _maskWidth :: Number,
    -- | Map to the `height` attribute.
    _maskHeight :: Number,
    -- | Children of the `<mask>` tag.
    _maskContent :: [Tree]
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg Mask where
  defaultSvg =
    Mask
      { _maskDrawAttributes = mempty,
        _maskContentUnits = CoordUserSpace,
        _maskUnits = CoordBoundingBox,
        _maskPosition = (Percent (-0.1), Percent (-0.1)),
        _maskWidth = Percent 1.2,
        _maskHeight = Percent 1.2,
        _maskContent = []
      }

-- | Define a `<clipPath>` tag.
data ClipPath = ClipPath
  { _clipPathDrawAttributes :: DrawAttributes,
    -- | Maps to the `clipPathUnits` attribute
    _clipPathUnits :: CoordinateUnits,
    -- | Maps to the content of the tree
    _clipPathContent :: [Tree]
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg ClipPath where
  defaultSvg =
    ClipPath
      { _clipPathDrawAttributes = mempty,
        _clipPathUnits = CoordUserSpace,
        _clipPathContent = mempty
      }

-- | Define a `<pattern>` tag.
data Pattern = Pattern
  { _patternDrawAttributes :: DrawAttributes,
    -- | Possible `viewBox`.
    _patternViewBox :: !(Maybe (Double, Double, Double, Double)),
    -- | Width of the pattern tile, mapped to the
    -- `width` attribute
    _patternWidth :: !Number,
    -- | Height of the pattern tile, mapped to the
    -- `height` attribute
    _patternHeight :: !Number,
    -- | Pattern tile base, mapped to the `x` and
    -- `y` attributes.
    _patternPos :: !Point,
    -- | Patterns can be chained, so this is a potential
    -- reference to another pattern
    _patternHref :: !String,
    -- | Elements used in the pattern.
    _patternElements :: ![Tree],
    -- | Define the cordinate system to use for
    -- the pattern. Mapped to the `patternUnits`
    -- attribute.
    _patternUnit :: !CoordinateUnits,
    -- | Value of the "preserveAspectRatio" attribute
    _patternAspectRatio :: !PreserveAspectRatio,
    -- | Value of "patternTransform" attribute
    _patternTransform :: !(Maybe [Transformation])
  }
  deriving (Eq, Show, Generic)

instance WithDefaultSvg Pattern where
  defaultSvg =
    Pattern
      { _patternDrawAttributes = mempty,
        _patternViewBox = Nothing,
        _patternWidth = Num 0,
        _patternHeight = Num 0,
        _patternPos = (Num 0, Num 0),
        _patternElements = [],
        _patternUnit = CoordBoundingBox,
        _patternAspectRatio = defaultSvg,
        _patternHref = "",
        _patternTransform = mempty
      }

-- | Sum types helping keeping track of all the namable
-- elemens in a SVG document.
data Element
  = ElementLinearGradient LinearGradient
  | ElementRadialGradient RadialGradient
  | ElementMeshGradient MeshGradient
  | ElementGeometry Tree
  | ElementPattern Pattern
  | ElementMarker Marker
  | ElementMask Mask
  | ElementClipPath ClipPath
  deriving (Eq, Show, Generic)

-- | Represent a full svg document with style,
-- geometry and named elements.
data Document = Document
  { _documentViewBox :: Maybe (Double, Double, Double, Double),
    _documentWidth :: Maybe Number,
    _documentHeight :: Maybe Number,
    _documentElements :: [Tree],
    _documentDescription :: String,
    _documentLocation :: FilePath,
    _documentAspectRatio :: PreserveAspectRatio
  }
  deriving (Show, Eq, Generic)

-- | Calculate the document size in function of the
-- different available attributes in the document.
documentSize :: Dpi -> Document -> (Int, Int)
documentSize
  _
  Document
    { _documentViewBox = Just (x1, y1, x2, y2),
      _documentWidth = Just (Percent pw),
      _documentHeight = Just (Percent ph)
    } =
    (floor $ dx * pw, floor $ dy * ph)
    where
      dx = abs $ x2 - x1
      dy = abs $ y2 - y1
documentSize
  _
  Document
    { _documentWidth = Just (Num w),
      _documentHeight = Just (Num h)
    } = (floor w, floor h)
documentSize
  dpi
  doc@( Document
          { _documentWidth = Just w,
            _documentHeight = Just h
          }
        ) =
    documentSize dpi $
      doc
        { _documentWidth = Just $ toUserUnit dpi w,
          _documentHeight = Just $ toUserUnit dpi h
        }
documentSize _ Document {_documentViewBox = Just (x1, y1, x2, y2)} =
  (floor . abs $ x2 - x1, floor . abs $ y2 - y1)
documentSize _ _ = (1, 1)

mayMerge :: Monoid a => Maybe a -> Maybe a -> Maybe a
mayMerge (Just a) (Just b) = Just $ mappend a b
mayMerge _ b@(Just _) = b
mayMerge a Nothing = a

instance Semigroup DrawAttributes where
  (<>) a b =
    DrawAttributes
      { _strokeWidth = chooseLast _strokeWidth,
        _strokeColor = chooseLast _strokeColor,
        _strokeLineCap = chooseLast _strokeLineCap,
        _strokeOpacity = (opacityMappend `on` _strokeOpacity) a b,
        _strokeLineJoin = chooseLast _strokeLineJoin,
        _strokeMiterLimit = chooseLast _strokeMiterLimit,
        _fillColor = chooseLast _fillColor,
        _fillOpacity = (opacityMappend `on` _fillOpacity) a b,
        _fontSize = chooseLast _fontSize,
        _transform = (mayMerge `on` _transform) a b,
        _fillRule = chooseLast _fillRule,
        _attrClass = _attrClass b,
        _attrId = _attrId b,
        _groupOpacity = _groupOpacity b,
        _strokeOffset = chooseLast _strokeOffset,
        _strokeDashArray = chooseLast _strokeDashArray,
        _fontFamily = chooseLast _fontFamily,
        _fontStyle = chooseLast _fontStyle,
        _textAnchor = chooseLast _textAnchor,
        _maskRef = chooseLast _maskRef,
        _clipPathRef = chooseLast _clipPathRef,
        _clipRule = chooseLast _clipRule,
        _markerStart = chooseLast _markerStart,
        _markerMid = chooseLast _markerMid,
        _markerEnd = chooseLast _markerEnd,
        _filterRef = chooseLast _filterRef
      }
    where
      opacityMappend Nothing Nothing = Nothing
      opacityMappend (Just v) Nothing = Just v
      opacityMappend Nothing (Just v) = Just v
      opacityMappend (Just v) (Just v2) = Just $ v * v2
      chooseLast f = f b <|> f a

instance Monoid DrawAttributes where
  mappend = (<>)
  mempty =
    DrawAttributes
      { _strokeWidth = Nothing,
        _strokeColor = Nothing,
        _strokeOpacity = Nothing,
        _strokeLineCap = Nothing,
        _strokeLineJoin = Nothing,
        _strokeMiterLimit = Nothing,
        _fillColor = Nothing,
        _groupOpacity = Nothing,
        _fillOpacity = Nothing,
        _fontSize = Nothing,
        _fontFamily = Nothing,
        _fontStyle = Nothing,
        _transform = Nothing,
        _fillRule = Nothing,
        _attrClass = mempty,
        _attrId = Nothing,
        _strokeOffset = Nothing,
        _strokeDashArray = Nothing,
        _textAnchor = Nothing,
        _maskRef = Nothing,
        _clipPathRef = Nothing,
        _clipRule = Nothing,
        _markerStart = Nothing,
        _markerMid = Nothing,
        _markerEnd = Nothing,
        _filterRef = Nothing
      }

instance WithDefaultSvg DrawAttributes where
  defaultSvg = mempty

--------------------------------------------------------------------------
--- Template Haskell and HasDrawAttributes instances
--------------------------------------------------------------------------

makeLenses ''Rectangle
makeLenses ''Pattern
makeLenses ''Document
makeLenses ''Filter
makeLenses ''Line
makeLenses ''Polygon
makeLenses ''PolyLine
makeLenses ''PreserveAspectRatio
makeLenses ''Path
makeLenses ''Circle
makeLenses ''Text
makeLenses ''TextPath
makeLenses ''Ellipse
makeLenses ''MeshGradientPatch
makeLenses ''MeshGradientRow
makeLenses ''MeshGradient
makeLenses ''Image
makeLenses ''Use
makeLenses ''TextSpan
makeLenses ''TextInfo
makeLenses ''Marker
makeLenses ''GradientStop
makeLenses ''LinearGradient
makeLenses ''RadialGradient
makeLenses ''Mask
makeLenses ''ClipPath
makeLenses ''Blend
makeLenses ''Flood
makeLenses ''Tile
makeLenses ''Offset
makeLenses ''ColorMatrix
makeLenses ''Composite
makeLenses ''GaussianBlur
makeLenses ''Turbulence
makeLenses ''DisplacementMap
makeLenses ''Merge
makeLenses ''MergeNode
makeLenses ''Group
makeLenses ''ImageF
makeLenses ''ComponentTransfer
makeLenses ''FuncA
makeLenses ''FuncR
makeLenses ''FuncG
makeLenses ''FuncB
makeLenses ''Morphology
makeLenses ''SpecularLighting
makeLenses ''DropShadow
makeLenses ''DiffuseLighting
makeLenses ''ConvolveMatrix

makeClassy ''FilterAttributes
