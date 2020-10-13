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

import           Data.Attoparsec.Text   hiding (Number)


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
-- patternTransform
-- patternUnits
-- ping
-- pointer-events
-- points
-- pointsAtX
-- pointsAtY
-- pointsAtZ
-- preserveAlpha
-- preserveAspectRatio
-- primitiveUnits

-- r
-- radius
-- referrerPolicy
-- refX
-- refY
-- rel
-- rendering-intent
-- repeatCount
-- repeatDur
-- requiredExtensions
-- requiredFeatures
-- restart
-- result
-- rotate
-- rx
-- ry

-- scale
-- seed
-- shape-rendering
-- slope
-- spacing
-- specularConstant
-- specularExponent
-- speed
-- spreadMethod
-- startOffset
-- stdDeviation
-- stemh
-- stemv
-- stitchTiles
-- stop-color
-- stop-opacity
-- strikethrough-position
-- strikethrough-thickness
-- string
-- stroke
-- stroke-dasharray
-- stroke-dashoffset
-- stroke-linecap
-- stroke-linejoin
-- stroke-miterlimit
-- stroke-opacity
-- stroke-width
-- style
-- surfaceScale
-- systemLanguage

-- tabindex
-- tableValues
-- target
-- targetX
-- targetY
-- text-anchor
-- text-decoration
-- text-rendering
-- textLength
-- to
-- transform
-- transform-origin
-- type

-- u1
-- u2
-- underline-position
-- underline-thickness
-- unicode
-- unicode-bidi
-- unicode-range
-- units-per-em

-- v-alphabetic
-- v-hanging
-- v-ideographic
-- v-mathematical
-- values
-- vector-effect
-- version
-- vert-adv-y
-- vert-origin-x
-- vert-origin-y
-- viewBox
viewBoxParser :: Parser ViewBox
viewBoxParser =  viewBox
       <$> iParse <*> iParse <*> iParse <*> iParse
  where
    iParse = numberParser <* skipSpace
    viewBox a b c d = ViewBox $ Just (a,b,c,d)

-- viewTarget
-- visibility

-- width
widthAttrParser :: Parser WidthAttr
widthAttrParser = choice
                 [ WidthAttrAuto <$ "auto"
                 , WidthAttr <$> lengthParser ]

-- widths
-- word-spacing
-- writing-mode

-- x
-- x-height
-- x1
-- x2
-- xChannelSelector
-- xlink:actuate
-- xlink:arcrole
-- xlink:href
-- xlink:role
-- xlink:show
-- xlink:title
-- xlink:type
-- xml:base
-- xml:lang
-- xml:space

-- y
-- y1
-- y2
-- yChannelSelector

-- z
-- zoomAndPan
