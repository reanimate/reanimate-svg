module Graphics.SvgTree.Parser.ParseableAttribute where

import Graphics.SvgTree.Types.Attributes
import Graphics.SvgTree.Parser.AttributesParser

import Data.Attoparsec.Text (Parser, parseOnly)
import qualified Data.Text as T


class ParseableAttribute a where
  parseAttribute :: T.Text -> Maybe a


parseA :: Parser a -> T.Text -> Maybe a
parseA p t = getRight $ parseOnly p t
  where
    getRight (Left _) = Nothing
    getRight (Right r) = Just r


-- accent-height
instance ParseableAttribute AccentHeight where
  parseAttribute = parseA accentHeightParser

-- accumulate
instance ParseableAttribute Accumulate where
  parseAttribute = parseA accumulateParser

-- additive
instance ParseableAttribute Additive where
  parseAttribute = parseA additiveParser

-- alignment-baseline
instance ParseableAttribute AlignementBaseline where
  parseAttribute = parseA alignementBaselineParser

-- allowReorder
-- TODO

-- alphabetic
instance ParseableAttribute Alphabetic where
  parseAttribute = parseA alphabeticParser

-- amplitude
instance ParseableAttribute Amplitude where
  parseAttribute = parseA amplitudeParser

-- arabic-form
instance ParseableAttribute ArabicForm where
  parseAttribute = parseA arabicFormParser

-- ascent
instance ParseableAttribute Accent where
  parseAttribute = parseA accentParser

-- attributeName
instance ParseableAttribute AttributeName where
  parseAttribute = parseA attributeNameParser

-- attributeTyp
instance ParseableAttribute AttributeType where
  parseAttribute = parseA attributeTypeParser

-- autoReverse
-- TODO

-- azimuth
instance ParseableAttribute Azimuth where
  parseAttribute = parseA azimuthParser

-- baseFrequency
-- TODO

-- baseline-shift
instance ParseableAttribute BaselineShift where
  parseAttribute = parseA baselineShiftParser

-- baseProfile
instance ParseableAttribute BaseProfile where
  parseAttribute = parseA baseProfileParser

-- bbox
-- TODO

-- begin
-- TODO

-- bias
instance ParseableAttribute Bias where
  parseAttribute = parseA biasParser

-- by
-- TODO

-- calcMode
instance ParseableAttribute CalcMode where
  parseAttribute = parseA calcModeParser

-- cap-height
instance ParseableAttribute CapHeight where
  parseAttribute = parseA capHeightParser

-- class
-- TODO

-- clip
-- TODO

-- clipPathUnits
instance ParseableAttribute ClipPathUnits where
  parseAttribute = parseA clipPathUnitsParser

-- clip-path
-- TODO

-- clip-rule
instance ParseableAttribute ClipRule where
  parseAttribute = parseA clipRuleParser

-- color
instance ParseableAttribute ColorAttr where
  parseAttribute = parseA colorAttrParser

-- color-interpolation
instance ParseableAttribute ColorInterpolation where
  parseAttribute = parseA colorInterpolationParser

-- color-interpolation-filters
instance ParseableAttribute ColorInterpolationFilters where
  parseAttribute = parseA colorInterpolationFilterParser

-- color-profile
-- TODO

-- color-rendering
instance ParseableAttribute ColorRendering where
  parseAttribute = parseA colorRenderingParser

-- contentScriptType
-- TODO

-- contentStyleType
-- TODO

-- cursor
-- TODO: not entirely right, the standard allows for zero or more FuncIRI fields.
-- value := [[<funciri>,]* [ auto | crosshair...
instance ParseableAttribute Cursor where
  parseAttribute = parseA cursorParser

-- cx
instance ParseableAttribute Cx where
  parseAttribute = parseA cxParser

-- cy
instance ParseableAttribute Cy where
  parseAttribute = parseA cyParser

-- d
-- TODO

-- decelerate
-- TODO

-- descent
instance ParseableAttribute Descent where
  parseAttribute = parseA descentParser

-- diffuseConstant
instance ParseableAttribute DiffuseConstant where
  parseAttribute = parseA diffuseConstantParser

-- direction
instance ParseableAttribute Direction where
  parseAttribute = parseA directionParser

-- display
-- TODO

-- divisor
instance ParseableAttribute Divisor where
  parseAttribute = parseA divisorParser

-- dominant-baseline
instance ParseableAttribute DominantBaseline where
  parseAttribute = parseA dominantBaselineParser

-- dur
-- TODO

-- dx
-- TODO

-- dy
-- TODO

-- edgeMode
instance ParseableAttribute EdgeMode where
  parseAttribute = parseA edgeModeParser

-- elevation
instance ParseableAttribute Elevation where
  parseAttribute = parseA elevationParser

-- enable-background
-- TODO

-- end
-- TODO

-- exponent
instance ParseableAttribute Exponent where
  parseAttribute = parseA exponentParser

-- externalResourcesRequired
instance ParseableAttribute ExternalResourcesRequired where
  parseAttribute = parseA externalResourcesRequiredParser

-- fill
-- TODO: different elements define different subsets of this attribute.
-- Better to parse in the elements to disallow invalid SVGs to be parsed?
instance ParseableAttribute FillAttr where
  parseAttribute = parseA fillParser

-- fill-opacity
instance ParseableAttribute FillOpacity where
  parseAttribute = parseA fillOpacityParser

-- fill-rule
instance ParseableAttribute FillRule where
  parseAttribute = parseA fillRuleParser

-- filter
-- TODO

-- filterRes
-- TODO

-- filterUnits
instance ParseableAttribute FilterUnits where
  parseAttribute = parseA filterUnitsParser

-- flood-color
instance ParseableAttribute FloodColor where
  parseAttribute = parseA floodColorParser

-- flood-opacity
instance ParseableAttribute FloodOpacity where
  parseAttribute = parseA floodOpacityParser

-- font-family
-- TODO: this is not correct. The standard allow for the names to be repeated (#).
-- value := [ <family-name> | <generic-family> ]#
instance ParseableAttribute FontFamily where
  parseAttribute = parseA fontFamilyParser

-- font-size
instance ParseableAttribute FontSize where
  parseAttribute = parseA fontSizeParser

-- font-size-adjust
instance ParseableAttribute FontSizeAdjust where
  parseAttribute = parseA fontSizeAdjustParser

-- font-stretch
instance ParseableAttribute FontStretch where
  parseAttribute = parseA fontStretchParser

-- font-style
instance ParseableAttribute FontStyle where
  parseAttribute = parseA fontStyleParser

-- font-variant
-- TODO

-- font-weight
instance ParseableAttribute FontWeight where
  parseAttribute = parseA fontWeightParser

-- format
instance ParseableAttribute Format where
  parseAttribute = parseA formatParser

-- from
-- TODO

-- fr
instance ParseableAttribute Fr where
  parseAttribute = parseA frParser

-- fx
instance ParseableAttribute Fx where
  parseAttribute = parseA fxParser

-- fy
instance ParseableAttribute Fy where
  parseAttribute = parseA fyParser

-- g1
-- TODO

-- g2
-- TODO

-- glyph-name
-- TODO

-- glyph-orientation-horizontal
instance ParseableAttribute GlyphOrientationHorizontal where
  parseAttribute = parseA glyphOrientationHorizontalParser

-- glyph-orientation-vertical
instance ParseableAttribute GlyphOrientationVertical where
  parseAttribute = parseA glyphOrientationVerticalParser

-- glyphRef
instance ParseableAttribute GlyphRef where
  parseAttribute = parseA glyphRefParser

-- gradientTransform
-- TODO

-- gradientUnits
instance ParseableAttribute GradientUnits where
  parseAttribute = parseA gradientUnitsParser

-- hanging
instance ParseableAttribute Hanging where
  parseAttribute = parseA hangingParser

-- height
instance ParseableAttribute HeightAttr where
  parseAttribute = parseA heightAttrParser

-- href
instance ParseableAttribute Href where
  parseAttribute = parseA hrefParser

-- hreflang
-- TODO

-- horiz-adv-x
instance ParseableAttribute HorizAdvX where
  parseAttribute = parseA horizAdvXParser

-- horiz-origin-x
instance ParseableAttribute HorizOriginX where
  parseAttribute = parseA horizOriginXParser

-- id
-- TODO

-- ideographic
instance ParseableAttribute Ideographic where
  parseAttribute = parseA ideographicParser

-- image-rendering
instance ParseableAttribute ImageRendering where
  parseAttribute = parseA imageRenderingParser

-- in
instance ParseableAttribute In where
  parseAttribute = parseA inParser

-- in2
instance ParseableAttribute In2 where
  parseAttribute = parseA in2Parser

-- intercept
instance ParseableAttribute Intercept where
  parseAttribute = parseA interceptParser

-- k
instance ParseableAttribute K where
  parseAttribute = parseA kParser

-- k1
instance ParseableAttribute K1 where
  parseAttribute = parseA k1Parser

-- k2
instance ParseableAttribute K2 where
  parseAttribute = parseA k2Parser

-- k3
instance ParseableAttribute K3 where
  parseAttribute = parseA k3Parser

-- k4
instance ParseableAttribute K4 where
  parseAttribute = parseA k4Parser

-- kernelMatrix
-- TODO

-- kernelUnitLength
-- TODO

-- kerning
instance ParseableAttribute Kerning where
  parseAttribute = parseA kerningParser

-- keyPoints
-- TODO

-- keySplines
-- TODO

-- keyTimes
-- TODO

-- lang
instance ParseableAttribute Lang where
  parseAttribute = parseA langParser

-- lengthAdjust
instance ParseableAttribute LengthAdjust where
  parseAttribute = parseA lenghtAdjustParser

-- letter-spacing
instance ParseableAttribute LetterSpacing where
  parseAttribute = parseA letterSpacingParser

-- lighting-color
instance ParseableAttribute LightingColor where
  parseAttribute = parseA lightingColorParser

-- limitingConeAngle
instance ParseableAttribute LimitingConeAngle where
  parseAttribute = parseA limitingConeAngleParser

-- local
instance ParseableAttribute Local where
  parseAttribute = parseA localParser

-- marker-end
instance ParseableAttribute MarkerEnd where
  parseAttribute = parseA markerEndParser

-- marker-mid
instance ParseableAttribute MarkerMid where
  parseAttribute = parseA markerMidParser

-- marker-start
instance ParseableAttribute MarkerStart where
  parseAttribute = parseA markerStartParser

-- markerHeight
instance ParseableAttribute MarkerHeight where
  parseAttribute = parseA markerHeightParser

-- markerUnits
instance ParseableAttribute MarkerUnits where
  parseAttribute = parseA markerUnitsParser

-- markerWidth
instance ParseableAttribute MarkerWidth where
  parseAttribute = parseA markerWidthParser

-- mask
-- TODO

-- maskContentUnits
instance ParseableAttribute MaskContentUnits where
  parseAttribute = parseA maskContentUnitsParser

-- maskUnits
instance ParseableAttribute MaskUnits where
  parseAttribute = parseA maskUnitsParser

-- mathematical
instance ParseableAttribute Mathematical where
  parseAttribute = parseA mathematicalParser

-- max
-- TODO

-- media
-- TODO

-- method
instance ParseableAttribute Method where
  parseAttribute = parseA methodParser

-- min
-- TODO

-- mode
instance ParseableAttribute Mode where
  parseAttribute = parseA modeParser

-- name
instance ParseableAttribute NameAttr where
  parseAttribute = parseA nameAttrParser

-- numOctaves
instance ParseableAttribute NumOctaves where
  parseAttribute = parseA numOctavesParser

-- offset
-- TODO

-- opacity
instance ParseableAttribute Opacity where
  parseAttribute = parseA opacityParser

-- operator
instance ParseableAttribute Operator where
  parseAttribute = parseA operatorParser

-- order
-- TODO

-- orient
instance ParseableAttribute Orient where
  parseAttribute = parseA orientParser

-- orientation
instance ParseableAttribute Orientation where
  parseAttribute = parseA orientationParser

-- origin
instance ParseableAttribute OriginAttr where
  parseAttribute = parseA originParser

-- overflow
instance ParseableAttribute Overflow where
  parseAttribute = parseA overflowParser

-- overline-position
instance ParseableAttribute OverlinePosition where
  parseAttribute = parseA overlinePositionParser

-- overline-thickness
instance ParseableAttribute OverlineThickness where
  parseAttribute = parseA overlineThicknessParser

-- panose-1
-- TODO

-- paint-order
-- TODO

-- path
-- TODO

-- pathLength
instance ParseableAttribute PathLength where
  parseAttribute = parseA pathLengthParser

-- patternContentUnits
instance ParseableAttribute PatternContentUnits where
  parseAttribute = parseA patternContentUnitsParser

-- patternTransform
-- TODO

-- patternUnits
instance ParseableAttribute PatternUnits where
  parseAttribute = parseA patternUnitsParser

-- ping
-- TODO

-- pointer-events
instance ParseableAttribute PointerEvents where
  parseAttribute = parseA pointerEventsParser

-- points
-- TODO

-- pointsAtX
instance ParseableAttribute PointsAtX where
  parseAttribute = parseA pointsAtXParser

-- pointsAtY
instance ParseableAttribute PointsAtY where
  parseAttribute = parseA pointsAtYParser

-- pointsAtZ
instance ParseableAttribute PointsAtZ where
  parseAttribute = parseA pointsAtZParser

-- preserveAlph
instance ParseableAttribute PreserveAlpha where
  parseAttribute = parseA preserveAlphaParser

-- preserveAspectRatio
-- TODO

-- primitiveUnits
instance ParseableAttribute PrimitiveUnits where
  parseAttribute = parseA primitiveUnitsParser

-- r
instance ParseableAttribute RAttr where
  parseAttribute = parseA rAttrParser

-- radius
-- TODO

-- referrerPolicy
-- TODO

-- refX
instance ParseableAttribute RefX where
  parseAttribute = parseA refXParser

-- refY
instance ParseableAttribute RefY where
  parseAttribute = parseA refYParser

-- rel
-- TODO

-- rendering-intent
instance ParseableAttribute RenderingIntent where
  parseAttribute = parseA renderingIntentParser

-- repeatCount
instance ParseableAttribute RepeatCount where
  parseAttribute = parseA repeatCountParser

-- repeatDur
-- TODO

-- requiredExtensions
-- TODO

-- requiredFeatures
-- TODO

-- restart
instance ParseableAttribute Restart where
  parseAttribute = parseA restartParser

-- rotate
instance ParseableAttribute  RotateAttr where
  parseAttribute = parseA rotateParser

-- rx
instance ParseableAttribute Rx where
  parseAttribute = parseA rxParser

-- ry
instance ParseableAttribute Ry where
  parseAttribute = parseA ryParser

-- scale
instance ParseableAttribute ScaleAttr where
  parseAttribute = parseA scaleParser

-- seed
instance ParseableAttribute Seed where
  parseAttribute = parseA seedParser

-- shape-rendering
instance ParseableAttribute ShapeRendering where
  parseAttribute = parseA shapeRenderingParser

-- slope
instance ParseableAttribute Slope where
  parseAttribute = parseA slopeParser

-- spacing
instance ParseableAttribute Spacing where
  parseAttribute = parseA spacingParser

-- specularConstant
instance ParseableAttribute SpecularConstant where
  parseAttribute = parseA specularConstantParser

-- specularExponent
instance ParseableAttribute SpecularExponent where
  parseAttribute = parseA specularExponentParser

-- speed
-- TODO

-- spreadMethod
instance ParseableAttribute SpreadMethod where
  parseAttribute = parseA spreadMethodParser

-- startOffset
instance ParseableAttribute StartOffset where
  parseAttribute = parseA startOffsetParser

-- stdDeviation
-- TODO

-- stemh
instance ParseableAttribute StemH where
  parseAttribute = parseA stemHParser

-- stemv
instance ParseableAttribute StemV where
  parseAttribute = parseA stemVParser

-- stitchTiles
instance ParseableAttribute StitchTiles where
  parseAttribute = parseA stitchTilesParser

-- stop-color
-- TODO

-- stop-opacity
instance ParseableAttribute StopOpacity where
  parseAttribute = parseA stopOpacityParser

-- strikethrough-position
instance ParseableAttribute StrikethroughPosition where
  parseAttribute = parseA strikethroughPositionParser

-- strikethrough-thickness
instance ParseableAttribute StrikethroughThickness where
  parseAttribute = parseA strikethroughThicknessParser

-- string
instance ParseableAttribute StringAttr where
  parseAttribute = parseA stringParser

-- stroke
-- TODO

-- stroke-dasharray
-- TODO

-- stroke-dashoffset
instance ParseableAttribute StrokeDashoffset where
  parseAttribute = parseA strokeDashoffsetParser

-- stroke-linecap
instance ParseableAttribute StrokeLinecap where
  parseAttribute = parseA strokeLinecapParser

-- stroke-linejoin
instance ParseableAttribute StrokeLineJoin where
  parseAttribute = parseA strokeLineJoinParser

-- stroke-miterlimit
instance ParseableAttribute StrokeMiterlimit where
  parseAttribute = parseA strokeMiterlimitParser

-- stroke-opacity
instance ParseableAttribute StrokeOpacity where
  parseAttribute = parseA strokeOpacityParser

-- stroke-width
instance ParseableAttribute StrokeWidth where
  parseAttribute = parseA strokeWidthParser

-- style
-- TODO

-- surfaceScale
instance ParseableAttribute SurfaceScale where
  parseAttribute = parseA surfaceScaleParser

-- systemLanguage
instance ParseableAttribute SystemLang where
  parseAttribute = parseA systemLangParser

-- tabindex
instance ParseableAttribute Tabindex where
  parseAttribute = parseA tabIndexParser

-- tableValues
-- TODO

-- target
instance ParseableAttribute Target where
  parseAttribute = parseA targetParser

-- targetX
instance ParseableAttribute TargetX where
  parseAttribute = parseA targetXParser

-- targetY
instance ParseableAttribute TargetY where
  parseAttribute = parseA targetYParser

-- text-anchor
instance ParseableAttribute TextAnchor where
  parseAttribute = parseA textAnchorParser

-- text-decoration
-- TODO

-- text-rendering
instance ParseableAttribute TextRendering where
  parseAttribute = parseA textRenderingParser

-- textLength
instance ParseableAttribute TextLength where
  parseAttribute = parseA textLengthParser

-- to
-- TODO

-- transform
-- TODO

-- transform-origin
-- TODO

-- type
instance ParseableAttribute TypeAttr where
  parseAttribute = parseA typeParser

-- u1
-- TODO

-- u2
-- TODO

-- underline-position
instance ParseableAttribute UnderlinePosition where
  parseAttribute = parseA underlinePositionParser

-- underline-thickness
instance ParseableAttribute UnderlineThickness where
  parseAttribute = parseA underlineThicknessParser

-- unicode
instance ParseableAttribute Unicode where
  parseAttribute = parseA unicodeParser

-- unicode-bidi
instance ParseableAttribute UnicodeBidi where
  parseAttribute = parseA unicodeBidiParser

-- unicode-range
-- TODO

-- units-per-em
instance ParseableAttribute UnitsPerEm where
  parseAttribute = parseA unitsPerEmParser

-- v-alphabetic
instance ParseableAttribute VAlphabetic where
  parseAttribute = parseA vAlphabeticParser

-- v-hanging
instance ParseableAttribute VHanging where
  parseAttribute = parseA vHangingParser

-- v-ideographic
instance ParseableAttribute VIdeographic where
  parseAttribute = parseA vIdeographicParser

-- v-mathematical
instance ParseableAttribute VMathematical where
  parseAttribute = parseA vMathematicalParser

-- values
-- TODO

-- vector-effect
instance ParseableAttribute VectorEffect where
  parseAttribute = parseA vectorEffectParser

-- version
instance ParseableAttribute Version where
  parseAttribute = parseA versionParser

-- vert-adv-y
instance ParseableAttribute VertAdvY where
  parseAttribute = parseA vertAdvYParser

-- vert-origin-x
instance ParseableAttribute VertOriginX where
  parseAttribute = parseA vertOriginXParser

-- vert-origin-y
instance ParseableAttribute VertOriginY where
  parseAttribute = parseA vertOriginYParser

-- viewBox
instance ParseableAttribute ViewBox where
  parseAttribute = parseA viewBoxParser

-- viewTarget
instance ParseableAttribute ViewTarget where
  parseAttribute = parseA viewTargetParser

-- visibility
instance ParseableAttribute Visibility where
  parseAttribute = parseA visibilityParser

-- width
instance ParseableAttribute WidthAttr where
  parseAttribute = parseA widthAttrParser

-- widths
instance ParseableAttribute Widths where
  parseAttribute = parseA widthsParser

-- word-spacing
instance ParseableAttribute WordSpacing where
  parseAttribute = parseA wordSpacingParser

-- writing-mode
instance ParseableAttribute  WritingMode where
  parseAttribute = parseA writingModeParser

-- x
-- TODO

-- x-height
instance ParseableAttribute XHeight where
  parseAttribute = parseA xHeightParser

-- x1
instance ParseableAttribute X1 where
  parseAttribute = parseA x1Parser

-- x2
instance ParseableAttribute X2 where
  parseAttribute = parseA x2Parser

-- xChannelSelector
instance ParseableAttribute XChannelSelector where
  parseAttribute = parseA xChannelSelectorParser

-- xlink:actuate
-- TODO

-- xlink:arcrole
-- TODO

-- xlink:href
-- TODO

-- xlink:role
-- TODO

-- xlink:show
instance ParseableAttribute XlinkShow where
  parseAttribute = parseA xlinkShowParser

-- xlink:title
instance ParseableAttribute XlinkTitle where
  parseAttribute = parseA xlinkTitleParser

-- xlink:type
instance ParseableAttribute XlinkType where
  parseAttribute = parseA xlinkTypeParser

-- xml:base
-- TODO

-- xml:lang
instance ParseableAttribute XmlLang where
  parseAttribute = parseA xmlLangParser

-- xml:space
instance ParseableAttribute XmlSpace where
  parseAttribute = parseA xmlSpaceParser

-- y
-- TODO

-- y1
instance ParseableAttribute Y1 where
  parseAttribute = parseA y1Parser

-- y2
instance ParseableAttribute Y2 where
  parseAttribute = parseA y2Parser

-- yChannelSelector
instance ParseableAttribute YChannelSelector where
  parseAttribute = parseA yChannelSelectorParser

-- z
instance ParseableAttribute ZAttr where
  parseAttribute = parseA zParser

-- zoomAndPan
instance ParseableAttribute ZoomAndPan where
  parseAttribute = parseA zoomAndPanParser
