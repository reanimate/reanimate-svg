{-# LANGUAGE OverloadedStrings #-}

module Graphics.SvgTree.Parser.AttributesParser where

import Graphics.SvgTree.Types
import Graphics.SvgTree.Types.Attributes
import Graphics.SvgTree.Types.Contents
import Graphics.SvgTree.Parser.ContentsParser

import           Data.Scientific        (toRealFloat)
import qualified Data.Text              as T
import           Control.Applicative    ((<|>))
import           Data.Functor

import           Data.Attoparsec.Text   hiding (Number, Result)


-- accent-height
accentHeightParser :: Parser AccentHeight
accentHeightParser = AccentHeight <$> numberParser

-- accumulate
accumulateParser :: Parser Accumulate
accumulateParser = choice
                   [ AccNone <$ "none"
                   , AccSum  <$ "sum" ]

-- additive
additiveParser :: Parser Additive
additiveParser = choice
                 [ AddReplace <$ "replace"
                 , AddSum     <$ "sum" ]

-- alignment-baseline
alignementBaselineParser :: Parser AlignementBaseline
alignementBaselineParser = choice
                           [ AlignAuto           <$ "auto"
                           , AlignBaseline       <$ "baseline"
                           , AlignBeforeEdge     <$ "before-edge"
                           , AlignTextBeforeEdge <$ "text-before-edge"
                           , AlignMiddle         <$ "middle"
                           , AlignCentral        <$ "central"
                           , AlignAfterEdge      <$ "after-edge"
                           , AlignTextAfterEdge  <$ "text-after-edge"
                           , AlignIdeographic    <$ "ideographic"
                           , AlignAlphabetic     <$ "alphabetic"
                           , AlignHanging        <$ "hanging"
                           , AlignMathematical   <$ "mathematical"
                           , AlignTop            <$ "top"
                           , AlignCenter         <$ "center"
                           , AlignBottom         <$ "bottom" ]

-- allowReorder
-- TODO

-- alphabetic
alphabeticParser :: Parser Alphabetic
alphabeticParser = Alphabetic <$> numberParser

-- amplitude
amplitudeParser :: Parser Amplitude
amplitudeParser = Amplitude <$> numberParser

-- arabic-form
arabicFormParser :: Parser ArabicForm
arabicFormParser = choice
                   [ ArabicInitial  <$ "initial"
                   , ArabicMedial   <$ "medial"
                   , ArabicTerminal <$ "terminal"
                   , ArabicIsolated <$ "isolated"
                   ]

-- ascent
accentParser :: Parser Accent
accentParser = Accent <$> numberParser

-- attributeName
attributeNameParser :: Parser AttributeName
attributeNameParser = AttributeName <$> nameParser

-- attributeTyp
attributeTypeParser :: Parser AttributeType
attributeTypeParser = choice
                      [ AttrCSS  <$ "CSS"
                      , AttrXML  <$ "XML"
                      , AttrAuto <$ "auto"]

-- autoReverse
-- TODO

-- azimuth
azimuthParser :: Parser Azimuth
azimuthParser = Azimuth <$> numberParser

-- baseFrequency
-- TODO

-- baseline-shift
baselineShiftParser :: Parser BaselineShift
baselineShiftParser = choice
                      [ BaselineShiftSub <$ "sub"
                      , BaselineShiftSuper <$ "super"
                      , BaselineShift <$> lengthParser]

-- baseProfile
baseProfileParser :: Parser BaseProfile
baseProfileParser = BaseProfile <$> nameParser

-- bbox
-- TODO

-- begin
-- TODO

-- bias
biasParser :: Parser Bias
biasParser = Bias <$> numberParser

-- by
-- TODO

-- calcMode
calcModeParser :: Parser CalcMode
calcModeParser = choice
                 [ CalcDiscrete <$ "discrete"
                 , CalcLinear   <$ "linear"
                 , CalcPaced    <$ "paced"
                 , CalcSpline   <$ "spline" ]

-- cap-height
capHeightParser :: Parser CapHeight
capHeightParser = CapHeight <$> numberParser

-- class
-- TODO

-- clip
-- TODO

-- clipPathUnits
clipPathUnitsParser :: Parser ClipPathUnits
clipPathUnitsParser = ClipPathUnits <$> unitsParser

-- clip-path
-- TODO

-- clip-rule
clipRuleParser :: Parser ClipRule
clipRuleParser = choice
                 [ ClipNonZero <$ "nonzero"
                 , ClipEvenOdd <$ "evenodd"
                 , ClipInherit <$ "inherit" ]

-- color
colorAttrParser :: Parser ColorAttr
colorAttrParser = choice
                  [ ColorAttr        <$> colorParser
                  , ColorAttrInherit <$ "inherit"]

-- color-interpolation
colorInterpolationParser :: Parser ColorInterpolation
colorInterpolationParser = choice
                           [ ColorInterpAuto      <$ "auto"
                           , ColorInterpSRGB      <$ "sRGB"
                           , ColorInterpLinearRGB <$ "linearRGB" ]

-- color-interpolation-filters
colorInterpolationFilterParser :: Parser ColorInterpolationFilters
colorInterpolationFilterParser = choice
                                 [ ColorInterpFiltersAuto      <$ "auto"
                                 , ColorInterpFiltersSRGB      <$ "sRGB"
                                 , ColorInterpFiltersLinearRGB <$ "linearRGB" ]

-- color-profile
-- TODO

-- color-rendering
colorRenderingParser :: Parser ColorRendering
colorRenderingParser = choice
                       [ ColorRendAuto            <$ "auto"
                       , ColorRendOptimizeSpeed   <$ "optimizeSpeed"
                       , ColorRendOptimizeQuality <$ "optimizeQuality"
                       ]

-- contentScriptType
-- TODO

-- contentStyleType
-- TODO

-- cursor
-- TODO: not entirely right, the standard allows for zero or more FuncIRI fields.
-- value := [[<funciri>,]* [ auto | crosshair...
cursorParser :: Parser Cursor
cursorParser = choice
               [ Cursor <$> funcIRIParser <* commaWsp <*> cursorTypeParser
               , CursorInherit <$ "inherit"
               ]
  where cursorTypeParser :: Parser CursorType
        cursorTypeParser = choice
                           [ CursorAuto      <$ "auto"
                           , CursorCrosshair <$ "crosshair"
                           , CursorDefault   <$ "default"
                           , CursorPointer   <$ "pointer"
                           , CursorMove      <$ "move"
                           , CursorEResize   <$ "e-resize"
                           , CursorNeResize  <$ "ne-resize"
                           , CursorNwResize  <$ "nw-resize"
                           , CursorNResize   <$ "n-resize"
                           , CursorSeResize  <$ "se-resize"
                           , CursorSwResize  <$ "sw-resize"
                           , CursorSResize   <$ "s-resize"
                           , CursorWResize   <$ "w-resize"
                           , CursorText      <$ "text"
                           , CurosrWait      <$ "wait"
                           , CursorHelp      <$ "help"
                           ]

-- cx
cxParser :: Parser Cx
cxParser = Cx <$> lengthParser

-- cy
cyParser :: Parser Cy
cyParser = Cy <$> lengthParser

-- d
-- TODO

-- decelerate
-- TODO

-- descent
descentParser :: Parser Descent
descentParser = Descent <$> numberParser

-- diffuseConstant
diffuseConstantParser :: Parser DiffuseConstant
diffuseConstantParser = DiffuseConstant <$> numberParser

-- direction
directionParser :: Parser Direction
directionParser = choice
                  [ LTR <$ "ltr"
                  , RTL <$ "rtl" ]

-- display
-- TODO

-- divisor
divisorParser :: Parser Divisor
divisorParser = Divisor <$> numberParser

-- dominant-baseline
dominantBaselineParser :: Parser DominantBaseline
dominantBaselineParser = choice
                         [ DomAuto         <$ "auto"
                         , DomTextBottom   <$ "text-bottom"
                         , DomAlphabetic   <$ "alphabetic"
                         , DomIdeographic  <$ "ideographic"
                         , DomMiddle       <$ "middle"
                         , DomCentral      <$ "central"
                         , DomMathematical <$ "mathematical"
                         , DomHanging      <$ "hanging"
                         , DomTextTop      <$ "text-top" ]

-- dur
-- TODO

-- dx
-- TODO

-- dy
-- TODO

-- edgeMode
edgeModeParser :: Parser EdgeMode
edgeModeParser = choice
                 [ EdgeDuplicate <$ "duplicate"
                 , EdgeWrap      <$ "wrap"
                 , EdgeNone      <$ "none"
                 ]

-- elevation
elevationParser :: Parser Elevation
elevationParser = Elevation <$> numberParser

-- enable-background
-- TODO

-- end
-- TODO

-- exponent
exponentParser :: Parser Exponent
exponentParser = Exponent <$> numberParser

-- externalResourcesRequired
externalResourcesRequiredParser :: Parser ExternalResourcesRequired
externalResourcesRequiredParser = choice
                                  [ ExternalResourcesRequired True  <$ "true"
                                  , ExternalResourcesRequired False <$ "false" ]

-- fill
-- TODO: different elements define different subsets of this attribute.
-- Better to parse in the elements to disallow invalid SVGs to be parsed?
fillParser :: Parser FillAttr
fillParser = choice
             [ FillColor <$> colorParser
             , FillFreeze <$ "freeze"
             , FillRemove <$ "remove" ]

-- fill-opacity
fillOpacityParser :: Parser FillOpacity
fillOpacityParser = choice
                    [ FillOpacity <$> numberParser
                    , FillOpacityPercent <$> percentageParser ]

-- fill-rule
fillRuleParser :: Parser FillRule
fillRuleParser = choice
                 [ FillNonZero <$ "nonzero"
                 , FillEvenOdd <$ "evenodd" ]

-- filter
-- TODO

-- filterRes
-- TODO

-- filterUnits
filterUnitsParser :: Parser FilterUnits
filterUnitsParser = FilterUnits <$> unitsParser

-- flood-color
floodColorParser :: Parser FloodColor
floodColorParser = FloodColor <$> colorParser

-- flood-opacity
floodOpacityParser :: Parser FloodOpacity
floodOpacityParser = choice
                     [ FloodOpacity <$> numberParser
                     , FloodOpacityPercent <$> percentageParser ]

-- font-family
-- TODO: this is not correct. The standard allow for the names to be repeated (#).
-- value := [ <family-name> | <generic-family> ]#
fontFamilyParser :: Parser FontFamily
fontFamilyParser = choice
                   [ FontFamily          <$> takeText  -- TODO: check this.
                   , FontFamilySerif     <$ "serif"
                   , FontFamilySansSerif <$ "sans-serif"
                   , FontFamilyCursive   <$ "cursive"
                   , FontFamilyFantasy   <$ "fantasy"
                   , FontFamilyMonospace <$ "monospace" ]

-- font-size
fontSizeParser :: Parser FontSize
fontSizeParser = choice
                 [ FontSizeXXSmall  <$ "xx-small"
                 , FontSizeXSmall   <$ "x-small"
                 , FontSizeSmall    <$ "small"
                 , FontSizeMedium   <$ "medium"
                 , FontSizeLarge    <$ "large"
                 , FontSizeXLarge   <$ "x-large"
                 , FontSizeXXLarge  <$ "xx-large"
                 , FontSizeXXXLarge <$ "xxx-large"
                 , FontSizeLarger   <$ "larger"
                 , FontSizeSmaller  <$ "smaller"
                 , FontSizePercent  <$> percentageParser
                 , FontSize         <$> lengthParser ]

-- font-size-adjust
fontSizeAdjustParser :: Parser FontSizeAdjust
fontSizeAdjustParser = choice
                       [ FontSizeAdjustNone <$ "none"
                       , FontSizeAdjust     <$> numberParser ]

-- font-stretch
fontStretchParser :: Parser FontStretch
fontStretchParser = choice
                    [ FontStretchNormal         <$ "normal"
                    , FontStretchUltraCondensed <$ "ultra-condensed"
                    , FontStretchExtraCondensed <$ "extra-condensed"
                    , FontStretchCondensed      <$ "condensed"
                    , FontStretchSemiCondensed  <$ "semi-condensed"
                    , FontStretchSemiExpanded   <$ "semi-expanded"
                    , FontStretchExpanded       <$ "expanded"
                    , FontStretchExtraExpanded  <$ "extra-expanded"
                    , FontStretchUltraExpanded  <$ "ultra-expanded"
                    , FontStretch               <$> percentageParser]

-- font-style
fontStyleParser :: Parser FontStyle
fontStyleParser = choice
                  [ FontStyleNormal  <$ "normal"
                  , FontStyleItalic  <$ "italic"
                  , FontStyleOblique <$ "oblique"
                  ]

-- font-variant
-- TODO

-- font-weight
fontWeightParser :: Parser FontWeight
fontWeightParser = choice
                   [ FontWeightNormal  <$ "normal"
                   , FontWeightBold    <$ "bold"
                   , FontWeightBolder  <$ "bolder"
                   , FontWeightLighter <$ "lighter"
                   , FontWeight        <$> numberParser ]

-- format
formatParser :: Parser Format
formatParser = choice
               [ FormatTruedocPfr       <$ "truedoc-pfr"
               , FormatEmbeddedOpentype <$ "embedded-opentype"
               , FormatType1            <$ "type-1"
               , FormatTruetype         <$ "truetype"
               , FormatOpentype         <$ "opentype"
               , FormatTruetypeGx       <$ "truetype-gx"
               , FormatSpeedo           <$ "speedo"
               , FormatIntellifont      <$ "intellifont" ]

-- from
-- TODO

-- fr
frParser :: Parser Fr
frParser = Fr <$> lengthParser

-- fx
fxParser :: Parser Fx
fxParser = Fx <$> lengthParser

-- fy
fyParser :: Parser Fy
fyParser = Fy <$> lengthParser

-- g1
-- TODO

-- g2
-- TODO

-- glyph-name
-- TODO

-- glyph-orientation-horizontal
glyphOrientationHorizontalParser :: Parser GlyphOrientationHorizontal
glyphOrientationHorizontalParser = GlyphOrientationHorizontal <$> angleParser

-- glyph-orientation-vertical
glyphOrientationVerticalParser :: Parser GlyphOrientationVertical
glyphOrientationVerticalParser = choice
                                 [ GlyphOrientationVerticalAuto <$ "auto"
                                 , GlyphOrientationVertical     <$> angleParser ]

-- glyphRef
glyphRefParser :: Parser GlyphRef
glyphRefParser = GlyphRef <$> takeText

-- gradientTransform
-- TODO

-- gradientUnits
gradientUnitsParser :: Parser GradientUnits
gradientUnitsParser = GradientUnits <$> unitsParser

-- hanging
hangingParser :: Parser Hanging
hangingParser = Hanging <$> numberParser

-- height
heightAttrParser :: Parser HeightAttr
heightAttrParser = choice
                   [ HeightAttrAuto <$ "auto"
                   , HeightAttr <$> lengthParser ]

-- href
hrefParser :: Parser Href
hrefParser = Href <$> urlParser

-- hreflang
-- TODO

-- horiz-adv-x
horizAdvXParser :: Parser HorizAdvX
horizAdvXParser = HorizAdvX <$> numberParser

-- horiz-origin-x
horizOriginXParser :: Parser HorizOriginX
horizOriginXParser = HorizOriginX <$> numberParser

-- id
-- TODO

-- ideographic
ideographicParser :: Parser Ideographic
ideographicParser = Ideographic <$> numberParser

-- image-rendering
imageRenderingParser :: Parser ImageRendering
imageRenderingParser = choice
                       [ ImageRenderingAuto            <$ "auto"
                       , ImageRenderingOptimizeSpeed   <$ "optimizeSpeed"
                       , ImageRenderingOptimizeQuality <$ "optimizeQuality"]

-- in
inParser :: Parser In
inParser = choice
           [ InSourceGraphic   <$ "SourceGraphic"
           , InSourceAlpha     <$ "SourceAlpha"
           , InBackgroundImage <$ "BackgroundImage"
           , InBackgroundAlpha <$ "BackgroundAlpha"
           , InFillPaint       <$ "FillPaint"
           , InStrokePaint     <$ "StrokePaint"
           , In                <$> takeText ]

-- in2
in2Parser :: Parser In2
in2Parser = choice
           [ In2SourceGraphic   <$ "SourceGraphic"
           , In2SourceAlpha     <$ "SourceAlpha"
           , In2BackgroundImage <$ "BackgroundImage"
           , In2BackgroundAlpha <$ "BackgroundAlpha"
           , In2FillPaint       <$ "FillPaint"
           , In2StrokePaint     <$ "StrokePaint"
           , In2                <$> takeText ]

-- intercept
interceptParser :: Parser Intercept
interceptParser = Intercept <$> numberParser

-- k
kParser :: Parser K
kParser = K <$> numberParser

-- k1
k1Parser :: Parser K1
k1Parser = K1 <$> numberParser

-- k2
k2Parser :: Parser K2
k2Parser = K2 <$> numberParser

-- k3
k3Parser :: Parser K3
k3Parser = K3 <$> numberParser

-- k4
k4Parser :: Parser K4
k4Parser = K4 <$> numberParser

-- kernelMatrix
-- TODO

-- kernelUnitLength
-- TODO

-- kerning
kerningParser :: Parser Kerning
kerningParser = choice
                [ KerningAuto <$ "auto"
                , Kerning     <$> lengthParser ]

-- keyPoints
-- TODO

-- keySplines
-- TODO

-- keyTimes
-- TODO

-- lang
langParser :: Parser Lang
langParser = Lang <$> takeText

-- lengthAdjust
lenghtAdjustParser :: Parser LengthAdjust
lenghtAdjustParser = choice
                     [ LengthAdjustSpacing          <$ "spacing"
                     , LengthAdjustSpacingAndGlyphs <$ "spacingAndGlyphs" ]

-- letter-spacing
letterSpacingParser :: Parser LetterSpacing
letterSpacingParser = choice
                      [ LetterSpacingNormal <$ "normal"
                      , LetterSpacing       <$> lengthParser ]

-- lighting-color
lightingColorParser :: Parser LightingColor
lightingColorParser = LightingColor <$> colorParser

-- limitingConeAngle
limitingConeAngleParser :: Parser LimitingConeAngle
limitingConeAngleParser = LimitingConeAngle <$> numberParser

-- local
localParser :: Parser Local
localParser = Local <$> takeText

-- marker-end
markerEndParser :: Parser MarkerEnd
markerEndParser = choice
                  [ MarkerEndNone <$ "none"
                  , MarkerEnd <$> takeText ]

-- marker-mid
markerMidParser :: Parser MarkerMid
markerMidParser = choice
                  [ MarkerMidNone <$ "none"
                  , MarkerMid <$> takeText ]

-- marker-start
markerStartParser :: Parser MarkerStart
markerStartParser = choice
                  [ MarkerStartNone <$ "none"
                  , MarkerStart <$> takeText ]

-- markerHeight
markerHeightParser :: Parser MarkerHeight
markerHeightParser = choice
                     [ MarkerHeightPercent <$> percentageParser
                     , MarkerHeight        <$> numberParser ]

-- markerUnits
markerUnitsParser :: Parser MarkerUnits
markerUnitsParser = choice
                    [ MarkerUnitsUserSpaceOnUse <$ "userSpaceOnUse"
                    , MarkerUnitsStrokeWidth    <$ "strokeWidth" ]

-- markerWidth
markerWidthParser :: Parser MarkerWidth
markerWidthParser = choice
                    [ MarkerWidthPercent <$> percentageParser
                    , MarkerWidth        <$> numberParser ]

-- mask
-- TODO

-- maskContentUnits
maskContentUnitsParser :: Parser MaskContentUnits
maskContentUnitsParser = MaskContentUnits <$> unitsParser

-- maskUnits
maskUnitsParser :: Parser MaskUnits
maskUnitsParser = MaskUnits <$> unitsParser

-- mathematical
mathematicalParser :: Parser Mathematical
mathematicalParser = Mathematical <$> numberParser

-- max
-- TODO

-- media
-- TODO

-- method
methodParser :: Parser Method
methodParser = choice
                [ MethodAlign   <$ "align"
                , MethodStretch <$ "stretch" ]

-- min
-- TODO

-- mode
modeParser :: Parser Mode
modeParser = choice
             [ ModeNormal     <$ "normal"
             , ModeMultiply   <$ "multiply"
             , ModeScreen     <$ "screen"
             , ModeOverlay    <$ "overlay"
             , ModeDarken     <$ "darken"
             , ModeLighten    <$ "lighten"
             , ModeColorDodge <$ "color-dodge"
             , ModeColorBurn  <$ "color-burn"
             , ModeHardLight  <$ "hard-light"
             , ModeSoftLight  <$ "soft-light"
             , ModeDifference <$ "difference"
             , ModeExclusion  <$ "exclusion"
             , ModeHue        <$ "hue"
             , ModeSaturation <$ "saturation"
             , ModeColor      <$ "color"
             , ModeLuminosity <$ "luminosity" ]

-- name
nameAttrParser :: Parser NameAttr
nameAttrParser = NameAttr <$> nameParser

-- numOctaves
numOctavesParser :: Parser NumOctaves
numOctavesParser = NumOctaves <$> integerParser

-- offset
-- TODO

-- opacity
opacityParser :: Parser Opacity
opacityParser = Opacity <$> numberParser

-- operator
operatorParser :: Parser Operator
operatorParser = choice
                 [ OperatorOver       <$ "over"
                 , OperatorIn         <$ "in"
                 , OperatorOut        <$ "out"
                 , OperatorAtop       <$ "atop"
                 , OperatorXor        <$ "xor"
                 , OperatorLighter    <$ "lighter"
                 , OperatorArithmetic <$ "arithmetic"
                 , OperatorErode      <$ "erode"
                 , OperatorDilate     <$ "dilate" ]

-- order
-- TODO

-- orient
orientParser :: Parser Orient
orientParser = choice
               [ OrientAuto             <$ "auto"
               , OrientAutoStartReverse <$ "auto-start-reverse"
               , OrientAngle            <$> angleParser
               , Orient                 <$> numberParser ]

-- orientation
orientationParser :: Parser Orientation
orientationParser = choice
                    [ OrientationH <$ "h"
                    , OrientationV <$ "v" ]

-- origin
originParser :: Parser OriginAttr
originParser = OriginDefault <$ "default"

-- overflow
overflowParser :: Parser Overflow
overflowParser = choice
                 [ OverflowVisible <$ "visible"
                 , OverflowHidden  <$ "hidden"
                 , OverflowScroll  <$ "scroll"
                 , OverflowAuto    <$ "auto" ]

-- overline-position
overlinePositionParser :: Parser OverlinePosition
overlinePositionParser = OverlinePosition <$> numberParser

-- overline-thickness
overlineThicknessParser :: Parser OverlineThickness
overlineThicknessParser = OverlineThickness <$> numberParser

-- panose-1
-- TODO

-- paint-order
-- TODO

-- path
-- TODO

-- pathLength
pathLengthParser :: Parser PathLength
pathLengthParser = PathLength <$> numberParser

-- patternContentUnits
patternContentUnitsParser :: Parser PatternContentUnits
patternContentUnitsParser = PatternContentUnits <$> unitsParser

-- patternTransform
-- TODO

-- patternUnits
patternUnitsParser :: Parser PatternUnits
patternUnitsParser = PatternUnits <$> unitsParser

-- ping
-- TODO

-- pointer-events
pointerEventsParser :: Parser PointerEvents
pointerEventsParser = choice
                      [ PointerEventsBoundingBox    <$ "bounding-box"
                      , PointerEventsVisiblePainted <$ "visiblePainted"
                      , PointerEventsVisibleFill    <$ "visibleFill"
                      , PointerEventsVisibleStroke  <$ "visibleStroke"
                      , PointerEventsVisible        <$ "visible"
                      , PointerEventsPainted        <$ "painted"
                      , PointerEventsFill           <$ "fill"
                      , PointerEventsStroke         <$ "stroke"
                      , PointerEventsAll            <$ "all"
                      , PointerEventsNone           <$ "none"
                      ]

-- points
-- TODO

-- pointsAtX
pointsAtXParser :: Parser PointsAtX
pointsAtXParser = PointsAtX <$> numberParser

-- pointsAtY
pointsAtYParser :: Parser PointsAtY
pointsAtYParser = PointsAtY <$> numberParser

-- pointsAtZ
pointsAtZParser :: Parser PointsAtZ
pointsAtZParser = PointsAtZ <$> numberParser

-- preserveAlph
preserveAlphaParser :: Parser PreserveAlpha
preserveAlphaParser = choice
                      [ PreserveAlpha True  <$ "true"
                      , PreserveAlpha False <$ "false" ]

-- preserveAspectRatio
-- TODO

-- primitiveUnits
primitiveUnitsParser :: Parser PrimitiveUnits
primitiveUnitsParser = PrimitiveUnits <$> unitsParser

-- r
rAttrParser :: Parser RAttr
rAttrParser = RAttr <$> lengthParser

-- radius
-- TODO

-- referrerPolicy
-- TODO

-- refX
refXParser :: Parser RefX
refXParser = choice
             [ RefXLeft   <$ "left"
             , RefXCenter <$ "center"
             , RefXRight  <$ "right"
             , RefXLength <$> lengthParser
             , RefX       <$> numberParser ]

-- refY
refYParser :: Parser RefY
refYParser = choice
             [ RefYTop    <$ "top"
             , RefYCenter <$ "center"
             , RefYBottom <$ "bottom"
             , RefYLength <$> lengthParser
             , RefY       <$> numberParser ]

-- rel
-- TODO

-- rendering-intent
renderingIntentParser :: Parser RenderingIntent
renderingIntentParser = choice
                        [ RenderingIntentAuto                 <$ "auto"
                        , RenderingIntentPerceptual           <$ "perceptual"
                        , RenderingIntentRelativeColorimetric <$ "relative-colorimetric"
                        , RenderingIntentSaturation           <$ "saturation"
                        , RenderingIntentAbsoluteColorimetric <$ "absolute-colorimetric" ]

-- repeatCount
repeatCountParser :: Parser RepeatCount
repeatCountParser = choice
                    [ RepeatCountIndefinite <$ "indefinite"
                    , RepeatCount           <$> numberParser ]

-- repeatDur
-- TODO

-- requiredExtensions
-- TODO

-- requiredFeatures
-- TODO

-- restart
restartParser :: Parser Restart
restartParser = choice
                [ RestartAlways        <$ "always"
                , RestartWhenNotActive <$ "whenNotActive"
                , RestartNever         <$ "never" ]

-- result

resultParser :: Parser Result
resultParser = Result <$> nameParser

-- rotate
rotateParser :: Parser  RotateAttr
rotateParser = choice
               [ RotateAttrAuto        <$ "auto"
               , RotateAttrAutoReverse <$ "auto-reverse"
               , RotateAttr            <$> numberParser ]

-- rx
rxParser :: Parser Rx
rxParser = choice
           [ RxAuto <$ "auto"
           , Rx     <$> lengthParser ]

-- ry
ryParser :: Parser Ry
ryParser = choice
           [ RyAuto <$ "auto"
           , Ry     <$> lengthParser ]

-- scale
scaleParser :: Parser ScaleAttr
scaleParser = ScaleAttr <$> numberParser

-- seed
seedParser :: Parser Seed
seedParser = Seed <$> numberParser

-- shape-rendering
shapeRenderingParser :: Parser ShapeRendering
shapeRenderingParser = choice
                       [ ShapeRenderingAuto               <$ "auto"
                       , ShapeRenderingOptimizeSpeed      <$ "optimizeSpeed"
                       , ShapeRenderingCripEdges          <$ "crispEdges"
                       , ShapeRenderingGeometricPrecision <$ "geometricPrecision" ]

-- slope
slopeParser :: Parser Slope
slopeParser = Slope <$> numberParser

-- spacing
spacingParser :: Parser Spacing
spacingParser = choice
                [ SpacingAuto  <$ "auto"
                , SpacingExact <$ "exact" ]

-- specularConstant
specularConstantParser :: Parser SpecularConstant
specularConstantParser = SpecularConstant <$> numberParser

-- specularExponent
specularExponentParser :: Parser SpecularExponent
specularExponentParser = SpecularExponent <$> numberParser

-- speed
-- TODO

-- spreadMethod
spreadMethodParser :: Parser SpreadMethod
spreadMethodParser = choice
                     [ SpreadMethodPad     <$ "pad"
                     , SpreadMethodReflect <$ "reflect"
                     , SpreadMethodRepeat  <$ "repeat" ]

-- startOffset
startOffsetParser :: Parser StartOffset
startOffsetParser = choice
                    [ StartOffsetLength <$> lengthParser
                    , StartOffset       <$> numberParser ]

-- stdDeviation
-- TODO

-- stemh
stemHParser :: Parser StemH
stemHParser = StemH <$> numberParser

-- stemv
stemVParser :: Parser StemV
stemVParser = StemV <$> numberParser

-- stitchTiles
stitchTilesParser :: Parser StitchTiles
stitchTilesParser = choice
                    [ NoStitch <$ "noStitch"
                    , Stitch   <$ "stitch" ]

-- stop-color
-- TODO

-- stop-opacity
stopOpacityParser :: Parser StopOpacity
stopOpacityParser = choice
                    [ StopOpacity        <$> numberParser
                    , StopOpacityPercent <$> percentageParser ]

-- strikethrough-position
strikethroughPositionParser :: Parser StrikethroughPosition
strikethroughPositionParser = StrikethroughPosition <$> numberParser

-- strikethrough-thickness
strikethroughThicknessParser :: Parser StrikethroughThickness
strikethroughThicknessParser = StrikethroughThickness <$> numberParser

-- string
stringParser :: Parser StringAttr
stringParser = StringAttr <$> anythingParser

-- stroke
-- strokeParser :: Parser Stroke
-- strokeParser = Stroke <$> paintParser

-- stroke-dasharray
-- TODO

-- stroke-dashoffset
strokeDashoffsetParser :: Parser StrokeDashoffset
strokeDashoffsetParser = choice
                         [ StrokeDashoffsetPercent <$> percentageParser
                         , StrokeDashoffset        <$> lengthParser ]

-- stroke-linecap
strokeLinecapParser :: Parser StrokeLinecap
strokeLinecapParser = choice
                      [ CapButt   <$ "butt"
                      , CapRound  <$ "round"
                      , CapSquare <$ "square" ]

-- stroke-linejoin
strokeLineJoinParser :: Parser StrokeLineJoin
strokeLineJoinParser = choice
                       [ JoinArcs      <$ "arcs"
                       , JoinBevel     <$ "bevel"
                       , JoinMiter     <$ "miter"
                       , JoinMiterClip <$ "miter-clip"
                       , JoinRound     <$ "round" ]

-- stroke-miterlimit
strokeMiterlimitParser :: Parser StrokeMiterlimit
strokeMiterlimitParser = StrokeMiterlimit <$> numberParser

-- stroke-opacity
strokeOpacityParser :: Parser StrokeOpacity
strokeOpacityParser = choice
                      [ StrokeOpacity        <$> numberParser
                      , StrokeOpacityPercent <$> percentageParser ]

-- stroke-width
strokeWidthParser :: Parser StrokeWidth
strokeWidthParser = StrokeWidth <$> lengthParser

-- style
-- TODO

-- surfaceScale
surfaceScaleParser :: Parser SurfaceScale
surfaceScaleParser = SurfaceScale <$> numberParser

-- systemLanguage
systemLangParser :: Parser SystemLang
systemLangParser = SystemLang <$> takeText

-- tabindex
tabIndexParser :: Parser Tabindex
tabIndexParser = Tabindex <$> integerParser

-- tableValues
-- TODO

-- target
targetParser :: Parser Target
targetParser = choice
               [ TargetSelf   <$ "_self"
               , TargetParent <$ "_parent"
               , TargetTop    <$ "_top"
               , TargetBlank  <$ "_blank"
               , Target       <$> nameParser ]

-- targetX
targetXParser :: Parser TargetX
targetXParser = TargetX <$> integerParser

-- targetY
targetYParser :: Parser TargetY
targetYParser = TargetY <$> integerParser

-- text-anchor
textAnchorParser :: Parser TextAnchor
textAnchorParser = choice
                   [ TextAnchorStart  <$ "start"
                   , TextAnchorMiddle <$ "middle"
                   , TextAnchorEnd    <$ "end" ]

-- text-decoration
-- TODO

-- text-rendering
textRenderingParser :: Parser TextRendering
textRenderingParser = choice
                      [ TextRenderingAuto               <$ "auto"
                      , TextRenderingOptimizeSpeed      <$ "optimizeSpeed"
                      , TextRenderingOptimizeLegibility <$ "optimizeLegibility"
                      , TextRenderingGeometricPrecision <$ "geometricPrecision" ]

-- textLength
textLengthParser :: Parser TextLength
textLengthParser = choice
                   [ TextLengthLength <$> lengthParser
                   , TextLength       <$> numberParser ]

-- to
-- TODO

-- transform
-- TODO

-- transform-origin
-- TODO

-- type
typeParser :: Parser TypeAttr
typeParser = choice
             [ TypeTranslate        <$ "translate"
             , TypeScale            <$ "scale"
             , TypeRotate           <$ "rotate"
             , TypeSkewX            <$ "skewX"
             , TypeSkewY            <$ "skewY"
             , TypeMatrix           <$ "matrix"
             , TypeSaturate         <$ "saturate"
             , TypeHueRotate        <$ "hueRotate"
             , TypeLuminanceToAlpha <$ "luminanceToAlpha"
             , TypeIdentity         <$ "identity"
             , TypeTable            <$ "table"
             , TypeDiscrete         <$ "discrete"
             , TypeLinear           <$ "linear"
             , TypeGamma            <$ "gamma"
             , TypeFractalNoise     <$ "fractalNoise"
             , TypeTurbulence       <$ "turbulence"
             , Type                 <$> takeText ]

-- u1
-- TODO

-- u2
-- TODO

-- underline-position
underlinePositionParser :: Parser UnderlinePosition
underlinePositionParser = UnderlinePosition <$> numberParser

-- underline-thickness
underlineThicknessParser :: Parser UnderlineThickness
underlineThicknessParser = UnderlineThickness <$> numberParser

-- unicode
unicodeParser :: Parser Unicode
unicodeParser = Unicode <$> takeText

-- unicode-bidi
unicodeBidiParser :: Parser UnicodeBidi
unicodeBidiParser = choice
                    [ UnicodeBidiNormal          <$ "normal"
                    , UnicodeBidiEmbed           <$ "embed"
                    , UnicodeBidiIsolate         <$ "isolate"
                    , UnicodeBidiBidiOverride    <$ "bidi-override"
                    , UnicodeBidiIsolateOverride <$ "isolate-override"
                    , UnicodeBidiPlaintext       <$ "plaintext" ]

-- unicode-range
-- TODO

-- units-per-em
unitsPerEmParser :: Parser UnitsPerEm
unitsPerEmParser = UnitsPerEm <$> numberParser

-- v-alphabetic
vAlphabeticParser :: Parser VAlphabetic
vAlphabeticParser = VAlphabetic <$> numberParser

-- v-hanging
vHangingParser :: Parser VHanging
vHangingParser = VHanging <$> numberParser

-- v-ideographic
vIdeographicParser :: Parser VIdeographic
vIdeographicParser = VIdeographic <$> numberParser

-- v-mathematical
vMathematicalParser :: Parser VMathematical
vMathematicalParser = VMathematical <$> numberParser

-- values
-- TODO

-- vector-effect
vectorEffectParser :: Parser VectorEffect
vectorEffectParser = choice
                     [ VectorEffectNone             <$ "none"
                     , VectorEffectNonScalingStroke <$ "non-scaling-stroke"
                     , VectorEffectNonScalingSize   <$ "non-scaling-size"
                     , VectorEffectNonRotation      <$ "non-rotation"
                     , VectorEffectFixedPosition    <$ "fixed-position" ]

-- version
versionParser :: Parser Version
versionParser = Version <$> numberParser

-- vert-adv-y
vertAdvYParser :: Parser VertAdvY
vertAdvYParser = VertAdvY <$> numberParser

-- vert-origin-x
vertOriginXParser :: Parser VertOriginX
vertOriginXParser = VertOriginX <$> numberParser

-- vert-origin-y
vertOriginYParser :: Parser VertOriginY
vertOriginYParser = VertOriginY <$> numberParser

-- viewBox
viewBoxParser :: Parser ViewBox
viewBoxParser =  viewBox
       <$> iParse <*> iParse <*> iParse <*> iParse
  where
    iParse = numberParser <* skipSpace
    viewBox a b c d = ViewBox $ Just (a,b,c,d)

-- viewTarget
viewTargetParser :: Parser ViewTarget
viewTargetParser = ViewTarget <$> nameParser

-- visibility
visibilityParser :: Parser Visibility
visibilityParser = choice
                   [ Visible  <$ "visible"
                   , Hidden   <$ "hidden"
                   , Collapse <$ "collapse" ]

-- width
widthAttrParser :: Parser WidthAttr
widthAttrParser = choice
                 [ WidthAttrAuto <$ "auto"
                 , WidthAttr <$> lengthParser ]

-- widths
widthsParser :: Parser Widths
widthsParser = Widths <$> numberParser

-- word-spacing
wordSpacingParser :: Parser WordSpacing
wordSpacingParser = choice
                    [ WordSpacingNormal <$ "normal"
                    , WordSpacing <$> lengthParser ]

-- writing-mode
writingModeParser :: Parser  WritingMode
writingModeParser = choice
                    [ WritingModeHorizontalTB <$ "horizontal-tb"
                    , WritingModeVerticalRL   <$ "vertical-rl"
                    , WritingModeVerticalLR   <$ "vertical-lr"]

-- x
-- TODO

-- x-height
xHeightParser :: Parser XHeight
xHeightParser = XHeight <$> numberParser

-- x1
x1Parser :: Parser X1
x1Parser = choice
           [ X1Percent <$> percentageParser
           , X1        <$> lengthParser ]

-- x2
x2Parser :: Parser X2
x2Parser = choice
           [ X2Percent <$> percentageParser
           , X2        <$> lengthParser ]

-- xChannelSelector
xChannelSelectorParser :: Parser XChannelSelector
xChannelSelectorParser = choice
                         [ XChannelR <$ "R"
                         , XChannelG <$ "G"
                         , XChannelB <$ "B"
                         , XChannelA <$ "A" ]

-- xlink:actuate
-- TODO

-- xlink:arcrole
-- TODO

-- xlink:href
-- TODO

-- xlink:role
-- TODO

-- xlink:show
xlinkShowParser :: Parser XlinkShow
xlinkShowParser = choice
                  [ XlinkShowNew     <$ "new"
                  , XlinkShowReplace <$ "replace"
                  , XlinkShowEmbed   <$ "embed"
                  , XlinkShowOther   <$ "other"
                  , XlinkShowNone    <$ "none" ]

-- xlink:title
xlinkTitleParser :: Parser XlinkTitle
xlinkTitleParser = XlinkTitle <$> anythingParser

-- xlink:type
xlinkTypeParser :: Parser XlinkType
xlinkTypeParser = XlinkTypeSimple <$ "simple"

-- xml:base
-- TODO

-- xml:lang
xmlLangParser :: Parser XmlLang
xmlLangParser = XmlLang <$> nameParser

-- xml:space
xmlSpaceParser :: Parser XmlSpace
xmlSpaceParser = choice
                 [ XmlSpaceDefault  <$ "default"
                 , XmlSpacePreserve <$ "preserve" ]

-- y
-- TODO

-- y1
y1Parser :: Parser Y1
y1Parser = choice
           [ Y1Percent <$> percentageParser
           , Y1        <$> lengthParser ]

-- y2
y2Parser :: Parser Y2
y2Parser = choice
           [ Y2Percent <$> percentageParser
           , Y2        <$> lengthParser ]

-- yChannelSelector
yChannelSelectorParser :: Parser YChannelSelector
yChannelSelectorParser = choice
                         [ YChannelR <$ "R"
                         , YChannelG <$ "G"
                         , YChannelB <$ "B"
                         , YChannelA <$ "A" ]
-- z
zParser :: Parser ZAttr
zParser = ZAttr <$> numberParser

-- zoomAndPan
zoomAndPanParser :: Parser ZoomAndPan
zoomAndPanParser = choice
                   [ ZoomAndPanDisable <$ "disable"
                   , ZoomAndPanMagnify <$ "magnify" ]
