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
-- elevation
-- enable-background
-- end
-- exponent
-- externalResourcesRequired

-- fill
-- fill-opacity
-- fill-rule
-- filter
-- filterRes
-- filterUnits
-- flood-color
-- flood-opacity
-- font-family
-- font-size
-- font-size-adjust
-- font-stretch
-- font-style
-- font-variant
-- font-weight
-- format
-- from
-- fr
-- fx
-- fy

-- g1
-- g2
-- glyph-name
-- glyph-orientation-horizontal
-- glyph-orientation-vertical
-- glyphRef
-- gradientTransform
-- gradientUnits

-- hanging
-- height
heightAttrParser :: Parser HeightAttr
heightAttrParser = choice
                   [ HeightAttrAuto <$ "auto"
                   , HeightAttr <$> lengthParser ]

-- href
-- hreflang
-- horiz-adv-x
-- horiz-origin-x

-- id
-- ideographic
-- image-rendering
-- in
-- in2
-- intercept
-- K
-- k
-- k1
-- k2
-- k3
-- k4
-- kernelMatrix
-- kernelUnitLength
-- kerning
-- keyPoints
-- keySplines
-- keyTimes

-- lang
-- lengthAdjust
-- letter-spacing
-- lighting-color
-- limitingConeAngle
-- local

-- marker-end
-- marker-mid
-- marker-start
-- markerHeight
-- markerUnits
-- markerWidth
-- mask
-- maskContentUnits
-- maskUnits
-- mathematical
-- max
-- media
-- method
-- min
-- mode
-- N
-- name
-- numOctaves

-- offset
-- opacity
-- operator
-- order
-- orient
-- orientation
-- origin
-- overflow
-- overline-position
-- overline-thickness

-- panose-1
-- paint-order
-- path
-- pathLength
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
