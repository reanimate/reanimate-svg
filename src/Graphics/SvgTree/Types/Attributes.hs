{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}


module Graphics.SvgTree.Types.Attributes where

--import Graphics.SvgTree.Types.Basic
--import Graphics.SvgTree.CssTypes
import Graphics.SvgTree.Types.Contents

import Control.Lens.TH (makeLenses, makeClassy)
import GHC.Generics (Generic)
import Data.Hashable
import qualified Data.Text as T

-- | Define a 'default' element for the SVG tree.
-- It is used as base when parsing the element from XML.
class WithDefaultSvg a where
  -- | The default element.
  defaultSvg :: a


-- Groups of attributes --

-- Core attributes
-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/Core
data CoreAttributes = CoreAttributes
  { -- _idA :: IdAttr,
    _langA     :: !(Maybe Lang),
    _tabIndexA :: !(Maybe Tabindex),
    -- _xmlBaseA  :: (Maybe XmlBase),
    _xmlLangA  :: !(Maybe XmlLang),
    _xmlSpaceA :: !(Maybe XmlSpace)
  }
  deriving (Eq, Show, Generic, Hashable)

instance WithDefaultSvg CoreAttributes where
  defaultSvg =
    CoreAttributes
    { -- _idA = ,
      _langA     = Nothing,
      _tabIndexA = Nothing,
      -- _xmlBaseA = Nothing,
      _xmlLangA  = Nothing,
      _xmlSpaceA = Nothing
    }


-- Style Attributes
--https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/Styling
data StylingAttributes = StylingAttributes
  { _classA :: !(Maybe Class)
    -- _styleA :: !(Maybe Style)
  }
  deriving (Eq, Show, Generic, Hashable)

instance WithDefaultSvg StylingAttributes where
  defaultSvg =
    StylingAttributes
    { _classA = Nothing --,
      --_styleA =
    }


-- Conditional Processing Attributes
data ConditionalProcessingAttributes = ConditionalProcessingAttributes
  { _externalResourcesRequiredA :: !(Maybe ExternalResourcesRequired),
    -- _requiredExtensionsA :: !(Maybe RequiredExtensions,)
    -- _requiredFeaturesA :: !(Maybe RequiredFeatures),
    _systemLanguageA :: !(Maybe SystemLang)
  }
  deriving(Eq, Show, Generic, Hashable)

instance WithDefaultSvg ConditionalProcessingAttributes where
  defaultSvg =
    ConditionalProcessingAttributes
    { _externalResourcesRequiredA = Nothing,
      -- _requiredExtensionsA
      -- _requiredFeaturesA
      _systemLanguageA = Nothing
    }


-- XLink Attributes
data XLinkAttributes = XLinkAttributes
  { --_xlinkHrefA ::
    _xlinkTypeA :: !(Maybe XlinkType),
    --_xlinkRole ::
    -- _xlinkArcrole ::
    _xlinkTitleA :: !(Maybe XlinkTitle),
    _xlinkShowA :: !(Maybe XlinkShow)
    --_xlinkActuate ::
  }
  deriving(Eq, Show, Generic, Hashable)

instance WithDefaultSvg XLinkAttributes where
  defaultSvg =
    XLinkAttributes
    { --_xlinkHrefA
      _xlinkTypeA = Nothing,
      --_xlinkRole
      -- _xlinkArcrole
      _xlinkTitleA = Nothing,
      _xlinkShowA = Nothing
      --_xlinkActuate ::
    }

-- Presentation Attributes
-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/Presentation
data PresentationAttributes = PresentationAttributes
  { _alignementBaselineA :: !(Maybe AlignementBaseline),
    _baselineShiftA :: !(Maybe BaselineShift),
    _clipA :: !(Maybe Clip),
    -- _clipPathA
    _clipRuleA :: !(Maybe ClipRule),
    _colorA :: !(Maybe ColorAttr),
    _colorInterpolationA :: !(Maybe ColorInterpolation),
    _colorInterpolationFiltersA :: !(Maybe ColorInterpolationFilters),
    _colorProfileA :: !(Maybe ColorProfile),
    _colorRenderingA :: !(Maybe ColorRendering),
    -- _cursorA
    _directionA :: !(Maybe Direction),
    -- _displayA
    _dominantBaselineA :: !(Maybe DominantBaseline),
    _enableBackgroundA :: !(Maybe EnableBackground),
    _fillA :: !(Maybe FillAttr),
    _fillOpacityA :: !(Maybe FillOpacity),
    _fillRuleA :: !(Maybe FillRule),
    -- _filterA
    _floodColorA :: !(Maybe FloodColor),
    _floodOpacityA :: !(Maybe FloodOpacity),
    -- _fontFamilyA
    _fontSizeA :: !(Maybe FontSize),
    _fontSizeAdjustA :: !(Maybe FontSizeAdjust),
    _fontStretchA :: !(Maybe FontStretch),
    _fontStyleA :: !(Maybe FontStyle),
    -- _FontVariantA :: !(Maybe )
    _fontWeightA :: !(Maybe FontWeight),
    _glyphOrientationHorizontalA :: !(Maybe GlyphOrientationHorizontal),
    _glyphOrientationVerticalA :: !(Maybe GlyphOrientationVertical),
    _imageRenderingA :: !(Maybe ImageRendering),
    _kerningA :: !(Maybe Kerning),
    _letterSpacingA :: !(Maybe LetterSpacing),
    _lightingColorA :: !(Maybe LightingColor),
    _markerEndA :: !(Maybe MarkerEnd),
    _markerMidA :: !(Maybe MarkerMid),
    _markerStartA :: !(Maybe MarkerStart),
    -- _maskA
    _opacityA :: !(Maybe Opacity),
    _overflowA :: !(Maybe Overflow),
    _pointerEventsA :: !(Maybe PointerEvents),
    _shapeRenderingA :: !(Maybe ShapeRendering),
    -- _solidColorA
    -- _solidOpacityA
    -- _stopColorA
    _stopOpacityA :: !(Maybe StopOpacity),
    _strokeA :: !(Maybe Stroke),
    _strokeDasharrayA :: !(Maybe StrokeDasharray),
    _strokeDashoffsetA :: !(Maybe StrokeDashoffset),
    _strokeLinecapA :: !(Maybe StrokeLinecap),
    _strokeLinejoinA :: !(Maybe StrokeLineJoin),
    _strokeMiterlimitA :: !(Maybe StrokeMiterlimit),
    _strokeOpacityA :: !(Maybe StrokeOpacity),
    _strokeWidthA :: !(Maybe StrokeWidth),
    _textAnchorA :: !(Maybe TextAnchor),
    -- _textDecorationA
    _textRenderingA :: !(Maybe TextRendering),
    _transformA :: !(Maybe TransformAttr),
    _unicodeBidiA :: !(Maybe UnicodeBidi),
    _vectorEffectA :: !(Maybe VectorEffect),
    _visibilityA :: !(Maybe Visibility),
    _wordSpacingA :: !(Maybe WordSpacing),
    _writingModeA :: !(Maybe WritingMode)
  }
  deriving(Eq, Show, Generic, Hashable)

instance WithDefaultSvg PresentationAttributes where
  defaultSvg =
    PresentationAttributes
    { _alignementBaselineA = Nothing,
      _baselineShiftA = Nothing,
      _clipA = Nothing,
    -- _clipPathA = Nothing,
      _clipRuleA = Nothing,
      _colorA = Nothing,
      _colorInterpolationA = Nothing,
      _colorInterpolationFiltersA = Nothing,
      _colorProfileA = Nothing,
      _colorRenderingA = Nothing,
    -- _cursorA = Nothing,
      _directionA = Nothing,
    -- _displayA =- Nothing,
      _dominantBaselineA = Nothing,
      _enableBackgroundA = Nothing,
      _fillA = Nothing,
      _fillOpacityA = Nothing,
      _fillRuleA = Nothing,
    -- _filterA = Nothing,
      _floodColorA = Nothing,
      _floodOpacityA = Nothing,
    -- _fontFamilyA = Nothing,
      _fontSizeA = Nothing,
      _fontSizeAdjustA = Nothing,
      _fontStretchA = Nothing,
      _fontStyleA = Nothing,
    -- _FontVariantA = Nothing,
      _fontWeightA = Nothing,
      _glyphOrientationHorizontalA = Nothing,
      _glyphOrientationVerticalA = Nothing,
      _imageRenderingA = Nothing,
      _kerningA = Nothing,
      _letterSpacingA = Nothing,
      _lightingColorA = Nothing,
      _markerEndA = Nothing,
      _markerMidA = Nothing,
      _markerStartA = Nothing,
    -- _maskA = Nothing,
      _opacityA = Nothing,
      _overflowA = Nothing,
      _pointerEventsA = Nothing,
      _shapeRenderingA = Nothing,
    -- _solidColorA = Nothing,
    -- _solidOpacityA = Nothing,
    -- _stopColorA = Nothing,
      _stopOpacityA = Nothing,
      _strokeA = Nothing,
      _strokeDasharrayA = Nothing,
      _strokeDashoffsetA = Nothing,
      _strokeLinecapA = Nothing,
      _strokeLinejoinA = Nothing,
      _strokeMiterlimitA = Nothing,
      _strokeOpacityA = Nothing,
      _strokeWidthA = Nothing,
      _textAnchorA = Nothing,
    -- _textDecorationA = Nothing,
      _textRenderingA = Nothing,
      _transformA = Nothing,
      _unicodeBidiA = Nothing,
      _vectorEffectA = Nothing,
      _visibilityA = Nothing,
      _wordSpacingA = Nothing,
      _writingModeA = Nothing
    }


-- Filter Primitive Attributes
data FilterPrimitiveAttributes = FilterPrimitiveAttributes
  { _heightA :: !(Maybe HeightAttr),
    _widthA :: !(Maybe WidthAttr),
    _xA :: !(Maybe XAttr),
    _yA :: !(Maybe YAttr),
    _resultA :: !(Maybe Result)
  }
  deriving(Eq, Show, Generic, Hashable)

instance WithDefaultSvg FilterPrimitiveAttributes where
  defaultSvg =
    FilterPrimitiveAttributes
    { _heightA = Nothing,
      _widthA = Nothing,
      _xA = Nothing,
      _yA = Nothing,
      _resultA = Nothing
    }


-- Transfer Function Attributes
data TransferFunctionAttributes = TransferFunctionAttributes
  { _typeA :: !(Maybe TypeAttr),
    _tableValuesA :: !(Maybe TableValues),
    _slopeA :: !(Maybe Slope),
    _interceptA :: !(Maybe Intercept),
    _amplitudeA :: !(Maybe Amplitude),
    _exponentA :: !(Maybe Exponent)
    --_offsetA ::
  }
  deriving(Eq, Show, Generic, Hashable)

instance WithDefaultSvg TransferFunctionAttributes where
  defaultSvg =
    TransferFunctionAttributes
    { _typeA = Nothing,
      _tableValuesA = Nothing,
      _slopeA = Nothing,
      _interceptA = Nothing,
      _amplitudeA = Nothing,
      _exponentA = Nothing
      --_offsetA = Nothing
    }


--data AnimationAttributeTargetAttributes
--data AnimationTimingAttributes
--data AnimationValueAttributes
--data AnimationAdditionAttributes


-- Attributes --

-- accent-height
newtype AccentHeight = AccentHeight Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg AccentHeight where
--   defaultSvg = AccentHeight $ Num 0

-- accumulate
data Accumulate
  = AccNone
  | AccSum
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Accumulate where
--   defaultSvg = AccSum

-- additive
data Additive
  = AddReplace
  | AddSum
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Additive where
--   defaultSvg = AddReplace

-- alignment-baseline
data AlignementBaseline
  = AlignAuto
  | AlignBaseline
  | AlignBeforeEdge
  | AlignTextBeforeEdge
  | AlignMiddle
  | AlignCentral
  | AlignAfterEdge
  | AlignTextAfterEdge
  | AlignIdeographic
  | AlignAlphabetic
  | AlignHanging
  | AlignMathematical
  | AlignTop
  | AlignCenter
  | AlignBottom
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg AlignementBaseline where
--   defaultSvg = AlignAuto

-- allowReorder                                   -- Definition is missing

-- alphabetic
newtype Alphabetic = Alphabetic Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Alphabetic where
--   defaultSvg = Alphabetic $ Num 0

-- amplitude
newtype Amplitude = Amplitude Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Amplitude where
--   defaultSvg = Amplitude $ Num 1

-- arabic-form
data ArabicForm
  = ArabicInitial
  | ArabicMedial
  | ArabicTerminal
  | ArabicIsolated
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg ArabicForm where
--   defaultSvg = ArabicIsolated

-- ascent
newtype Accent = Accent Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Accent where
--   defaultSvg = Accent $ Num 1000

-- attributeName
newtype AttributeName = AttributeName Name
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg AttributeName where
--   defaultSvg = AttributeName $ Name ""

-- attributeType
data AttributeType
  = AttrCSS
  | AttrXML
  | AttrAuto
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg AttributeType where
--   defaultSvg = AttrAuto

-- autoReverse                                    -- Definition is missing

-- azimuth
newtype Azimuth = Azimuth Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Azimuth where
--   defaultSvg = Azimuth $ Num 0

-- baseFrequency
newtype BaseFrequency = BaseFrequency NumberOptionalNumber
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg BaseFrequency where
--   defaultSvg = BaseFrequency $ NumOpt (Num 0) Nothing

-- baseline-shift
data BaselineShift
  = BaselineShiftSub
  | BaselineShiftSuper
  | BaselineShift Length
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg BaselineShift where
--   defaultSvg = BaselineShift $ Length 0

-- baseProfile
newtype BaseProfile = BaseProfile Name
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg BaseProfile where
--   defaultSvg = BaseProfile $ Name ""

-- bbox
data BBox = BBox Number Number Number Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg BBox where
--   defaultSvg = BBox ""

-- begin
-- TODO: this is broken, the type constructors should carry additional information.
data BeginAttr
  = BeginOffsetValue
  | BeginSyncBaseValue
  | BeginEventValue
  | BeginRepeatValue
  | BeginAccessKeyValue
  | BeginWallclockSyncValue
  | BeginIndefinite
  deriving (Eq, Show, Generic, Hashable)


-- bias
newtype Bias = Bias Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Bias where
--   defaultSvg = Bias $ Num 0

-- by
-- TODO: broken, see standard definition
data By = By
  deriving (Eq, Show, Generic, Hashable)

-- calcMode
data CalcMode
  = CalcDiscrete
  | CalcLinear
  | CalcPaced
  | CalcSpline
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg CalcMode where
--   defaultSvg = CalcLinear

-- cap-height
newtype CapHeight = CapHeight Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg CapHeight where
--   defaultSvg = CapHeight 0 --More appropriate would be Nothing

-- class
data Class = Class (ListOfTs Name)
  deriving (Eq, Show, Generic, Hashable)

-- clip
data Clip
  = ClipAuto
  | ClipRect Number Number Number Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Clip where
--   defaultSvg = ClipAuto

-- clipPathUnits
newtype ClipPathUnits = ClipPathUnits Units
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg ClipPathUnits where
--   defaultSvg = ClipPathUnits UserSpaceOnUse

-- clip-path
data ClipPath
  = ClipPath URL
  | ClipPathBasicShape BasicShape (Maybe ClipPathBox)
  | ClipPathNone
  deriving (Eq, Show, Generic, Hashable)

data ClipPathBox
  = ClipPathFillBox
  | ClipPathStrokeBox
  | ClipPathViewBox
  deriving (Eq, Show, Generic, Hashable)

-- clip-rule
data ClipRule
  = ClipNonZero
  | ClipEvenOdd
  | ClipInherit -- Is this necessary?
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg ClipRule where
--   defaultSvg = ClipNonZero

-- color
data ColorAttr
  = ColorAttr Color
  | ColorAttrInherit
  deriving (Eq, Show, Generic, Hashable)

-- color-interpolation
data ColorInterpolation
  = ColorInterpAuto
  | ColorInterpSRGB
  | ColorInterpLinearRGB
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg ColorInterpolation where
--   defaultSvg = ColorInterpSRGB

-- color-interpolation-filters
data ColorInterpolationFilters
  = ColorInterpFiltersAuto
  | ColorInterpFiltersSRGB
  | ColorInterpFiltersLinearRGB
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg ColorInterpolationFilters where
--   defaultSvg = ColorInterpFiltersSRGB

-- color-profile
data ColorProfile
  = ColorProfileAuto
  | ColorProfileSRGB
  | ColorProfileName Name
  | ColorProfileIRI String                        -- TODO: Change to IRI?
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg ColorProfile where
--   defaultSvg = ColorProfileAuto

-- color-rendering
data ColorRendering
  = ColorRendAuto
  | ColorRendOptimizeSpeed
  | ColorRendOptimizeQuality
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg ColorRendering where
--   defaultSvg = ColorRendAuto

-- contentScriptType
-- TODO: using string, but requires further analysis of the standard.
newtype ContentScriptType = ContentScriptType String
  deriving (Eq, Show, Generic, Hashable)

-- contentStyleType
-- TODO: using string, but requires further analysis of the standard.
newtype ContentStyleType = ContentStyleType String
  deriving (Eq, Show, Generic, Hashable)

-- cursor
-- TODO: not entirely right, the standard allows for zero or more FuncIRI fields.
-- value := [[<funciri>,]* [ auto | crosshair...
data Cursor
  = Cursor FuncIRI CursorType
  | CursorInherit
  deriving (Eq, Show, Generic, Hashable)

data CursorType
  = CursorAuto
  | CursorCrosshair
  | CursorDefault
  | CursorPointer
  | CursorMove
  | CursorEResize
  | CursorNeResize
  | CursorNwResize
  | CursorNResize
  | CursorSeResize
  | CursorSwResize
  | CursorSResize
  | CursorWResize
  | CursorText
  | CurosrWait
  | CursorHelp
  deriving (Eq, Show, Generic, Hashable)

-- cx
newtype Cx = Cx Length
  deriving (Eq, Show, Generic, Hashable)

-- cy
newtype Cy = Cy Length
  deriving (Eq, Show, Generic, Hashable)

-- d                                              -- TODO

-- decelerate                                     -- Definition is missing

-- descent
newtype Descent = Descent Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Descent where
--   defaultSvg = Descent $ Num 1000

-- diffuseConstant
newtype DiffuseConstant = DiffuseConstant Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg DiffuseConstant where
--   defaultSvg = DiffuseConstant $ Num 1

-- direction
data Direction
  = LTR
  | RTL
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Direction where
--   defaultSvg = LTR

-- display                                        -- TODO
data Display
  = Display (Maybe DisplayOutside) (Maybe DisplayInside)
  | DisplayListItem                               -- TODO
  | DisplayTableRowGroup
  | DisplayTableHeaderGroup
  | DisplayTableFooterGroup
  | DisplayTableRow
  | DisplayTableCell
  | DisplayTableColumnGroup
  | DisplayTableColumn
  | DisplayTableCaption
  | DisplayRubyBase
  | DisplayRubyText
  | DisplayRubyBaseContainer
  | DisplayRubyTextContainer
  | DisplayContents
  | DisplayNone
  | DisplayInlineBlock
  | DisplayInlineListItem
  | DisplayInlineTable
  | DisplayInlineFlex
  | DisplayInlineGrid
  deriving (Eq, Show, Generic, Hashable)

data DisplayOutside
  = DisplayOutsideBlock
  | DisplayOutsideInline
  | DisplayOutsideRunIn
  deriving (Eq, Show, Generic, Hashable)

data DisplayInside
  = DisplayInsideFlow
  | DisplayInsideFlowRoot
  | DisplayInsideTable
  | DisplayInsideFlex
  | DisplayInsideGrid
  | DisplayInsideRuby
  deriving (Eq, Show, Generic, Hashable)

-- divisor
newtype Divisor = Divisor Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Divisor where
--   defaultSvg = Divisor $ Num 1

-- dominant-baseline
data DominantBaseline
  = DomAuto
  | DomTextBottom
  | DomAlphabetic
  | DomIdeographic
  | DomMiddle
  | DomCentral
  | DomMathematical
  | DomHanging
  | DomTextTop
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg DominantBaseline where
--   defaultSvg = DomAuto

-- dur
data Dur
  = Dur ClockValue
  | DurMedia
  | DurIndefinite
  deriving (Eq, Show, Generic, Hashable)


-- dx
data Dx
  = DxList (ListOfTs Length)
  | DxNumber Number
  deriving (Eq, Show, Generic, Hashable)

-- dy
data Dy
  = DyList (ListOfTs Length)
  | DyNumber Number
  deriving (Eq, Show, Generic, Hashable)

-- edgeMode
data EdgeMode
  = EdgeDuplicate
  | EdgeWrap
  | EdgeNone
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg EdgeMode where
--   defaultSvg = EdgeNone

-- elevation
newtype Elevation = Elevation Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Elevation where
--   defaultSvg = Elevation $ Num 0

-- enable-background
data EnableBackground
  = BackgroundAccumulate
  | BackgroundNew (Maybe Number) (Maybe Number) (Maybe Number) (Maybe Number)
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg EnableBackground where
--   defaultSvg = BackgroundAccumulate

-- end
-- TODO: this is broken, the type constructors should carry additional information.
data EndAttr
  = EndOffsetValue
  | EndSyncBaseValue
  | EndEventValue
  | EndRepeatValue
  | EndAccessKeyValue
  | EndWallclockSyncValue
  | EndIndefinite
  deriving (Eq, Show, Generic, Hashable)


-- exponent
newtype Exponent = Exponent Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Exponent where
--   defaultSvg = Exponent $ Num 1

-- externalResourcesRequired
newtype ExternalResourcesRequired = ExternalResourcesRequired Bool
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg ExternalResourcesRequired where
--   defaultSvg = ExternalResourcesRequired False

-- fill
data FillAttr
  = FillColor Color
  | FillFreeze
  | FillRemove
  deriving (Eq, Show, Generic, Hashable)

-- fill-opacity
data FillOpacity
  = FillOpacity Number
  | FillOpacityPercent Percentage
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg FillOpacity where
--   defaultSvg = FillOpacity $ Num 1

-- fill-rule
data FillRule
    = FillNonZero -- ^ Correspond to the `nonzero` value.
    | FillEvenOdd -- ^ Correspond to the `evenodd` value.
    deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg FillRule where
--   defaultSvg = FillNonZero

-- filter                                         -- TODO
data FilterAttr
  = FilterAttrNone
  | FilterAttrURL URL
  | FilterAttr (ListOfTs FilterFunction)
  deriving (Eq, Show, Generic, Hashable)

-- TODO: these data types should carry information
data FilterFunction
  = FilterAttrBlur
  | FilterAttrBrightness
  | FilterAttrContrast
  | FilterAttrDropShadow
  | FilterAttrGrayscale
  | FilterAttrHueRotate
  | FilterAttrInvert
  | FilterAttrOpacity
  | FilterAttrSaturate
  | FilterAttrSepia
  deriving (Eq, Show, Generic, Hashable)

-- filterRes
data FilterRes = FilterRes NumberOptionalNumber
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg FilterRes where
--   defaultSvg = FilterRes Nothing Nothing

-- filterUnits
newtype FilterUnits = FilterUnits Units
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg FilterUnits where
--   defaultSvg = FilterUnits $ ObjectBoundingBox

-- flood-color
newtype FloodColor = FloodColor Color
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg FloodColor where
--   defaultSvg = FloodColor $ mkColor 0 0 0

-- flood-opacity
data FloodOpacity
  = FloodOpacity Number
  | FloodOpacityPercent Percentage
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg FloodOpacity where
--   defaultSvg = FloodOpacityNum $ Num 1

-- font-family
-- TODO: this is not correct. The standard allow for the names to be repeated (#).
-- value := [ <family-name> | <generic-family> ]#
data FontFamily
  = FontFamily T.Text  -- TODO: check this.
  | FontFamilySerif
  | FontFamilySansSerif
  | FontFamilyCursive
  | FontFamilyFantasy
  | FontFamilyMonospace
  deriving (Eq, Show, Generic, Hashable)

-- font-size
data FontSize
  = FontSizeXXSmall
  | FontSizeXSmall
  | FontSizeSmall
  | FontSizeMedium
  | FontSizeLarge
  | FontSizeXLarge
  | FontSizeXXLarge
  | FontSizeXXXLarge
  | FontSizeLarger
  | FontSizeSmaller
  | FontSizePercent Percentage
  | FontSize Length
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg FontSize where
--   defaultSvg = FontSizeMedium

-- font-size-adjust
data FontSizeAdjust
  = FontSizeAdjustNone
  | FontSizeAdjust Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg FontSizeAdjust where
--   defaultSvg = FontSizeAdjustNone

-- font-stretch
data FontStretch
  = FontStretchNormal
  | FontStretchUltraCondensed
  | FontStretchExtraCondensed
  | FontStretchCondensed
  | FontStretchSemiCondensed
  | FontStretchSemiExpanded
  | FontStretchExpanded
  | FontStretchExtraExpanded
  | FontStretchUltraExpanded
  | FontStretch Percentage
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg FontStretch where
--   defaultSvg = FontStretchNormal

-- font-style
data FontStyle
  = FontStyleNormal
  | FontStyleItalic
  | FontStyleOblique
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg FontStyle where
--   defaultSvg = FontStyleNormal

-- font-variant                                   -- TODO

-- font-weight
data FontWeight
  = FontWeightNormal
  | FontWeightBold
  | FontWeightBolder
  | FontWeightLighter
  | FontWeight Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg FontWeight where
--   defaultSvg = FontWeightNormal

-- format
data Format
  = FormatTruedocPfr
  | FormatEmbeddedOpentype
  | FormatType1
  | FormatTruetype
  | FormatOpentype
  | FormatTruetypeGx
  | FormatSpeedo
  | FormatIntellifont
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Format where
--   defaultSvg = Format (Nothing)

-- from                                           -- TODO

-- fr
newtype Fr = Fr Length
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Fr where
--   defaultSvg = Fr $ Length 0

-- fx
newtype Fx = Fx Length
  deriving (Eq, Show, Generic, Hashable)

-- fy
newtype Fy = Fy Length
  deriving (Eq, Show, Generic, Hashable)

-- g1                                             -- TODO

-- g2                                             -- TODO

-- glyph-name                                     -- TODO

-- glyph-orientation-horizontal
newtype GlyphOrientationHorizontal = GlyphOrientationHorizontal Angle
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg GlyphOrientationHorizontal where
--   defaultSvg = GlyphOrientationHorizontal $ Deg 0

-- glyph-orientation-vertical
data GlyphOrientationVertical
  = GlyphOrientationVertical Angle
  | GlyphOrientationVerticalAuto
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg GlyphOrientationVertical where
--   defaultSvg = GlyphOrientationVerticalAuto

-- glyphRef
newtype GlyphRef = GlyphRef T.Text
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg GlyphRef where
--   defaultSvg = GlyphRef ""

-- gradientTransform                              -- TODO

-- gradientUnits
newtype GradientUnits = GradientUnits Units
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg GradientUnits where
--   defaultSvg = GradientUnits $ ObjectBoundingBox

-- hanging
newtype Hanging = Hanging Number
  deriving (Eq, Show, Generic, Hashable)

--instance WithDefaultSvg Hanging where
--  defaultSvg = 0

-- height
data HeightAttr
  = HeightAttrAuto
  | HeightAttr Length
  deriving (Eq, Show, Generic, Hashable)

-- href
newtype Href = Href URL
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Href where
--   defaultSvg = Href $ URL ""

-- hreflang                                       -- Definition is missing

-- horiz-adv-x
newtype HorizAdvX = HorizAdvX Number
  deriving (Eq, Show, Generic, Hashable)

-- horiz-origin-x
newtype HorizOriginX = HorizOriginX Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg HorizOriginX where
--   defaultSvg = HorizOriginX $ Num 0

-- id                                             -- TODO
-- data IdAttr

-- ideographic
newtype Ideographic = Ideographic Number          -- No default?
  deriving (Eq, Show, Generic, Hashable)

-- image-rendering
data ImageRendering
  = ImageRenderingAuto
  | ImageRenderingOptimizeSpeed
  | ImageRenderingOptimizeQuality
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg ImageRendering where
--   defaultSvg = ImageRenderingAuto

-- in
data In                                           -- No default?
  = InSourceGraphic
  | InSourceAlpha
  | InBackgroundImage
  | InBackgroundAlpha
  | InFillPaint
  | InStrokePaint
  | In T.Text                                     -- Change to Name or URL?
  deriving (Eq, Show, Generic, Hashable)

-- in2
data In2                                          -- No default?
  = In2SourceGraphic
  | In2SourceAlpha
  | In2BackgroundImage
  | In2BackgroundAlpha
  | In2FillPaint
  | In2StrokePaint
  | In2 T.Text
  deriving (Eq, Show, Generic, Hashable)

-- intercept
newtype Intercept = Intercept Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Intercept where
--   defaultSvg = Intercept $ Num 0

-- k
newtype K = K Number
  deriving (Eq, Show, Generic, Hashable)

-- k1
newtype K1 = K1 Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg K1 where
--   defaultSvg = K1 $ Num 0

-- k2
newtype K2 = K2 Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg K2 where
--   defaultSvg = K2 $ Num 0

-- k3
newtype K3 = K3 Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg K3 where
--   defaultSvg = K3 $ Num 0

-- k4
newtype K4 = K4 Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg K4 where
--   defaultSvg = K4 $ Num 0

-- kernelMatrix
newtype KernelMatrix = KernelMatrix (ListOfTs Number)
  deriving (Eq, Show, Generic, Hashable)

-- kernelUnitLength
newtype KernelUnitLength = KernelUnitLength NumberOptionalNumber
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg KernelUnitLength where
--   defaultSvg = KernelUnitLength $ NumOpt (Num 0) Nothing

-- kerning
data Kerning
  = KerningAuto
  | Kerning Length
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Kerning where
--   defaultSvg = KerningAuto

-- keyPoints                                      -- TODO

-- keySplines                                     -- TODO

-- keyTimes                                       -- TODO

-- lang
data Lang = Lang String
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Lang where
--   defaultSvg = Lang ""

-- lengthAdjust
data LengthAdjust
  = LengthAdjustSpacing
  | LengthAdjustSpacingAndGlyphs
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg LengthAdjust where
--   defaultSvg = LengthAdjustSpacing

-- letter-spacing
data LetterSpacing
  = LetterSpacingNormal
  | LetterSpacing Length
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg LetterSpacing where
--   defaultSvg = LetterSpacingNormal

-- lighting-color
newtype LightingColor = LightingColor Color
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg LightingColor where
--   defaultSvg = LightingColor $ mkColor 255 255 255

-- limitingConeAngle
newtype LimitingConeAngle = LimitingConeAngle Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg LimitingConeAngle where
--   defaultSvg = LimitingConeAngle $ Num 0

-- local
newtype Local = Local String
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Local where
--   defaultSvg = Local ""

-- marker-end
data MarkerEnd
  = MarkerEndNone
  | MarkerEnd String                              -- Better use URL?
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg MarkerEnd where
--   defaultSvg = MarkerEndNone

-- marker-mid
data MarkerMid
  = MarkerMidNone
  | MarkerMid String                              -- Better use URL?
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg MarkerMid where
--   defaultSvg = MarkerMidNone

-- marker-start
data MarkerStart
  = MarkerStartNone
  | MarkerStart String                            -- Better use URL?
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg MarkerStart where
--   defaultSvg = MarkerStartNone

-- markerHeight
data MarkerHeight
  = MarkerHeightPercent Percentage
  | MarkerHeight Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg MarkerHeight where
--   defaultSvg = MarkerHeight $ Num 3

-- markerUnits
data MarkerUnits
  = MarkerUnitsUserSpaceOnUse
  | MarkerUnitsStrokeWidth
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg MarkerUnits where
--   defaultSvg = MarkerUnitsStrokeWidth

-- markerWidth
data MarkerWidth
  = MarkerWidthPercent Percentage
  | MarkerWidth Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg MarkerWidth where
--   defaultSvg = MarkerWidth $ Num 3

-- mask                                           -- TODO

-- maskContentUnits
newtype MaskContentUnits = MaskContentUnits Units
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg MaskContentUnits where
--   defaultSvg = MaskContentUnits $ UserSpaceOnUse

-- maskUnits
newtype MaskUnits = MaskUnits Units
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg MaskUnits where
--   defaultSvg = MaskUnits $ ObjectBoundingBox

-- mathematical
newtype Mathematical = Mathematical Number -- No default?
  deriving (Eq, Show, Generic, Hashable)

-- max                                            -- TODO

-- media                                          -- TODO

-- method
data Method
  = MethodAlign
  | MethodStretch
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Method where
--   defaultSvg = MethodAlign

-- min                                            -- TODO

-- mode
data Mode
  = ModeNormal
  | ModeMultiply
  | ModeScreen
  | ModeOverlay
  | ModeDarken
  | ModeLighten
  | ModeColorDodge
  | ModeColorBurn
  | ModeHardLight
  | ModeSoftLight
  | ModeDifference
  | ModeExclusion
  | ModeHue
  | ModeSaturation
  | ModeColor
  | ModeLuminosity
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Mode where
--   defaultSvg = ModeNormal

-- name
newtype NameAttr = NameAttr Name
  deriving (Eq, Show, Generic, Hashable)

-- numOctaves
newtype NumOctaves = NumOctaves SVGInteger
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg NumOctaves where
--   defaultSvg = NumOctaves $ SVGInteger 1

-- offset                                         -- Definition is missing

-- opacity
newtype Opacity = Opacity Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Opacity where
--   defaultSvg = Opacity $ Num 1

-- operator
data Operator
  = OperatorOver
  | OperatorIn
  | OperatorOut
  | OperatorAtop
  | OperatorXor
  | OperatorLighter
  | OperatorArithmetic
  | OperatorErode
  | OperatorDilate
  deriving (Eq, Show, Generic, Hashable)

-- order
newtype Order = Order NumberOptionalNumber
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Order where
--   defaultSvg = Order $ NumOpt (Num 3) Nothing

-- orient
data Orient
  = OrientAuto
  | OrientAutoStartReverse
  | OrientAngle Angle
  | Orient Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Orient where
--   defaultSvg = Orient $ Num 0

-- orientation
data Orientation                                  -- No default?
  = OrientationH
  | OrientationV
  deriving (Eq, Show, Generic, Hashable)

-- origin
data OriginAttr = OriginDefault
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg OriginAttr where
--   defaultSvg = OriginDefault

-- overflow
data Overflow
  = OverflowVisible
  | OverflowHidden
  | OverflowScroll
  | OverflowAuto
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Overflow where
--   defaultSvg = OverflowVisible

-- overline-position
newtype OverlinePosition = OverlinePosition Number
  deriving (Eq, Show, Generic, Hashable)

-- overline-thickness
newtype OverlineThickness = OverlineThickness Number
  deriving (Eq, Show, Generic, Hashable)

-- panose-1
data Panose1 = Panose1
                 SVGInteger
                 SVGInteger
                 SVGInteger
                 SVGInteger
                 SVGInteger
                 SVGInteger
                 SVGInteger
                 SVGInteger
                 SVGInteger
                 SVGInteger
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Panose1 where
--   defaultSvg = Panose1
--                  (SVGInteger 0)
--                  (SVGInteger 0)
--                  (SVGInteger 0)
--                  (SVGInteger 0)
--                  (SVGInteger 0)
--                  (SVGInteger 0)
--                  (SVGInteger 0)
--                  (SVGInteger 0)
--                  (SVGInteger 0)
--                  (SVGInteger 0)

-- paint-order
data PaintOrder
  = PaintOrderNormal
  | PaintOrder [PaintOrderType]
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg PaintOrder where
--   defaultSvg = PaintOrderNormal

data PaintOrderType
  = PaintOrderFill
  | PaintOrderStroke
  | PaintOrderMarkers
  deriving (Eq, Show, Generic, Hashable)

-- path                                           -- TODO

-- pathLength
newtype PathLength = PathLength Number
  deriving (Eq, Show, Generic, Hashable)

-- patternContentUnits
newtype PatternContentUnits = PatternContentUnits Units
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg PatternContentUnits where
--   defaultSvg = PatternContentUnits $ UserSpaceOnUse

-- patternTransform
newtype PatternTransform = PatternTransform TransformList
  deriving (Eq, Show, Generic, Hashable)

-- patternUnits
newtype PatternUnits = PatternUnits Units
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg PatternUnits where
--   defaultSvg = PatternUnits $ ObjectBoundingBox

-- ping                                           -- Definition is missing

-- pointer-events
data PointerEvents
  = PointerEventsBoundingBox
  | PointerEventsVisiblePainted
  | PointerEventsVisibleFill
  | PointerEventsVisibleStroke
  | PointerEventsVisible
  | PointerEventsPainted
  | PointerEventsFill
  | PointerEventsStroke
  | PointerEventsAll
  | PointerEventsNone
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg PointerEvents where
--   defaultSvg = PointerEventsVisiblePainted

-- points                                         -- TODO

-- pointsAtX
newtype PointsAtX = PointsAtX Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg PointsAtX where
--   defaultSvg = PointsAtX $ Num 0

-- pointsAtY
newtype PointsAtY = PointsAtY Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg PointsAtY where
--   defaultSvg = PointsAtY $ Num 0

-- pointsAtZ
newtype PointsAtZ = PointsAtZ Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg PointsAtZ where
--   defaultSvg = PointsAtZ $ Num 0

-- preserveAlpha
newtype PreserveAlpha = PreserveAlpha Bool
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg PreserveAlpha where
--   defaultSvg = PreserveAlpha False

-- preserveAspectRatio
data Alignment
  = AlignNone -- ^ "none" value
  | AlignxMinYMin -- "xMinYMin" value
  | AlignxMidYMin -- ^ "xMidYMin" value
  | AlignxMaxYMin -- ^ "xMaxYMin" value
  | AlignxMinYMid -- ^ "xMinYMid" value
  | AlignxMidYMid -- ^ "xMidYMid" value
  | AlignxMaxYMid -- ^ "xMaxYMid" value
  | AlignxMinYMax -- ^ "xMinYMax" value
  | AlignxMidYMax -- ^ "xMidYMax" value
  | AlignxMaxYMax -- ^ "xMaxYMax" value
  deriving (Eq, Show, Generic, Hashable)

data MeetSlice = Meet | Slice
  deriving (Eq, Show, Generic, Hashable)

data PreserveAspectRatio = PreserveAspectRatio
  { _aspectRatioDefer :: !Bool,
    _aspectRatioAlign :: !Alignment,
    _aspectRatioMeetSlice :: !(Maybe MeetSlice)
  }
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg PreserveAspectRatio where
--   defaultSvg =
--     PreserveAspectRatio
--       { _aspectRatioDefer = False,
--         _aspectRatioAlign = AlignxMidYMid,
--         _aspectRatioMeetSlice = Nothing
--       }

-- primitiveUnits
newtype PrimitiveUnits = PrimitiveUnits Units
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg PrimitiveUnits where
--   defaultSvg = PrimitiveUnits $ UserSpaceOnUse

-- r
newtype RAttr = RAttr Length
  deriving (Eq, Show, Generic, Hashable)

-- radius
newtype Radius = Radius NumberOptionalNumber
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Radius where
--   defaultSvg = Radius $ NumOpt (Num 0) Nothing

-- referrerPolicy                                 -- Definition is missing

-- refX
data RefX
  = RefXLeft
  | RefXCenter
  | RefXRight
  | RefXLength Length
  | RefX Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg RefX where
--   defaultSvg = RefX $ Num 0

-- refY
data RefY
  = RefYLeft
  | RefYCenter
  | RefYRight
  | RefYLength Length
  | RefY Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg RefY where
--   defaultSvg = RefY $ Num 0

-- rel                                            -- Definition is missing.

-- rendering-intent
data RenderingIntent
  = RenderingIntentAuto
  | RenderingIntentPerceptual
  | RenderingIntentRelativeColorimetric
  | RenderingIntentSaturation
  | RenderingIntentAbsoluteColorimetric
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg RenderingIntent where
--   defaultSvg = RenderingIntentAuto

-- repeatCount
data RepeatCount
  = RepeatCountIndefinite
  | RepeatCount Number
  deriving (Eq, Show, Generic, Hashable)

-- repeatDur                                      -- TODO

-- requiredExtensions                             -- Definition is missing

-- requiredFeatures                               -- TODO
-- newtype RequiredFeatures = RequiredFeatures (ListOfTs Name)
--   deriving (Eq, Show, Generic, Hashable)

-- restart
data Restart
  = RestartAlways
  | RestartWhenNotActive
  | RestartNever
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Restart where
--   defaultSvg = RestartAlways

-- result
newtype Result = Result Name
  deriving (Eq, Show, Generic, Hashable)

-- rotate
data RotateAttr
  = RotateAttrAuto
  | RotateAttrAutoReverse
  | RotateAttr Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg RotateAttr where
--   defaultSvg = RotateAttr $ Num 0

-- rx
data Rx
  = RxAuto
  | Rx Length
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Rx where
--   defaultSvg = RxAuto

-- ry
data Ry
  = RyAuto
  | Ry Length
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Ry where
--   defaultSvg = RyAuto

-- scale
newtype ScaleAttr = ScaleAttr Number
  deriving (Eq, Show, Generic, Hashable)

-- seed
newtype Seed = Seed Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Seed where
--   defaultSvg = Seed $ Num 0

-- shape-rendering
data ShapeRendering
  = ShapeRenderingAuto
  | ShapeRenderingOptimizeSpeed
  | ShapeRenderingCripEdges
  | ShapeRenderingGeometricPrecision
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg ShapeRendering where
--   defaultSvg = ShapeRenderingAuto

-- slope
newtype Slope = Slope Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Slope where
--   defaultSvg = Slope $ Num 0

-- spacing
data Spacing
  = SpacingAuto
  | SpacingExact
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Spacing where
--   defaultSvg = SpacingExact

-- specularConstant
newtype SpecularConstant = SpecularConstant Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg SpecularConstant where
--   defaultSvg = SpecularConstant $ Num 1

-- specularExponent
newtype SpecularExponent = SpecularExponent Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg SpecularExponent where
--   defaultSvg = SpecularExponent $ Num 1

-- speed                                          -- Definition is missing

-- spreadMethod
data SpreadMethod
  = SpreadMethodPad
  | SpreadMethodReflect
  | SpreadMethodRepeat
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg SpreadMethod where
--   defaultSvg = SpreadMethodPad

-- startOffset
data StartOffset
  = StartOffsetLength Length
  | StartOffset Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg StartOffset where
--   defaultSvg = StartOffset $ Num 0

-- stdDeviation
newtype StdDeviation = StdDeviation NumberOptionalNumber
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg StdDeviation where
--   defaultSvg = StdDeviation $ NumOpt (Num 0) Nothing

-- stemh
newtype SetmH = StemH Number
  deriving (Eq, Show, Generic, Hashable)

-- stemv
newtype SetmV = StemV Number
  deriving (Eq, Show, Generic, Hashable)

-- stitchTiles
data StitchTiles
  = NoStitch
  | Stitch
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg StitchTiles where
--   defaultSvg = NoStitch

-- stop-color                                     -- TODO

-- stop-opacity
data StopOpacity
  = StopOpacity Number
  | StopOpacityPercent Percentage
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg StopOpacity where
--   defaultSvg = StopOpacity $ Num 1

-- strikethrough-position
newtype StrikethroughPosition = StrikethroughPosition Number
  deriving (Eq, Show, Generic, Hashable)

-- strikethrough-thickness
newtype StrikethroughThickness = StrikethroughThickness Number
  deriving (Eq, Show, Generic, Hashable)

-- string
newtype StringAttr = StringAttr Anything
  deriving (Eq, Show, Generic, Hashable)

-- stroke
newtype Stroke = Stroke Paint
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Stroke where
--   defaultSvg = Stroke $ PaintNone

-- stroke-dasharray
data StrokeDasharray
  = StrokeDasharrayNone
  | StrokeDahsarray (ListOfTs Length)
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg StrokeDasharray where
--   defaultSvg = StrokeDasharrayNone

-- stroke-dashoffset
data StrokeDashoffset
  = StrokeDashoffsetPercent Percentage
  | StrokeDashoffset Length
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg StrokeDashoffset where
--   defaultSvg = StrokeDashoffset $ Length 0

-- stroke-linecap
data StrokeLinecap
  = CapButt  -- ^ Define straight just at the end (`butt` value)
  | CapRound -- ^ End with a round (`round` value)
  | CapSquare -- ^ Straight further of the ends (`square` value)
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg StrokeLinecap where
--   defaultSvg = CapButt

-- stroke-linejoin
data StrokeLineJoin
    = JoinArcs
    | JoinBevel -- ^ `bevel` value
    | JoinMiter -- ^ `miter` value
    | JoinMiterClip
    | JoinRound -- ^ `round` value
    deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg StrokeLineJoin where
--   defaultSvg = JoinMiter

type LineJoin = StrokeLineJoin

-- stroke-miterlimit
newtype StrokeMiterlimit = StrokeMiterlimit Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg StrokeMiterlimit where
--   defaultSvg = StrokeMiterlimit $ Num 4

-- stroke-opacity
data StrokeOpacity
  = StrokeOpacity Number
  | StrokeOpacityPercent Percentage
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg StrokeOpacity where
--   defaultSvg = StrokeOpacity $ Num 1

-- stroke-width
newtype StrokeWidth = StrokeWidth Length
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg StrokeWidth where
--   defaultSvg = StrokeWidth $ Px 1

-- style                                          -- TODO

-- surfaceScale
newtype SurfaceScale = SurfaceScale Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg SurfaceScale where
--   defaultSvg = SurfaceScale $ Num 1

-- systemLanguage
newtype SystemLang = SystemLang String
  deriving (Eq, Show, Generic, Hashable)

-- tabindex
newtype Tabindex = TabIndex SVGInteger
  deriving (Eq, Show, Generic, Hashable)

-- tableValues
newtype TableValues = TableValues (ListOfTs Number)
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg TableValues where
--   defaultSvg = TableValues $ ListOfTs []

-- target
data Target
  = TargetSelf
  | TargetParent
  | TargetTop
  | TargetBlank
  | Target Name
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Target where
--   defaultSvg = TargetSelf

-- targetX
newtype TargetX = TargetX SVGInteger
  deriving (Eq, Show, Generic, Hashable)

-- targetY
newtype TargetY = TargetY SVGInteger
  deriving (Eq, Show, Generic, Hashable)

-- text-anchor
data TextAnchor
  = TextAnchorStart
  | TextAnchorMiddle
  | TextAnchorEnd
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg TextAnchor where
--   defaultSvg = TextAnchorStart

-- text-decoration                                -- TODO

-- text-rendering
data TextRendering
  = TextRenderingAuto
  | TextRenderingOptimizeSpeed
  | TextRenderingOptimizeLegibility
  | TextRenderingGeometricPrecision
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg TextRendering where
--   defaultSvg = TextRenderingAuto

-- textLength
data TextLength
  = TextLengthLength Length
  | TextLength Number
  deriving (Eq, Show, Generic, Hashable)

-- to                                             -- TODO

-- transform
newtype TransformAttr = TransformAttr TransformList
  deriving (Eq, Show, Generic, Hashable)

-- transform-origin                               -- Definition is missing.

-- type
data TypeAttr
  = TypeTranslate
  | TypeScale
  | TypeRotate
  | TypeSkewX
  | TypeSkewY
  | TypeMatrix
  | TypeSaturate
  | TypeHueRotate
  | TypeLuminanceToAlpha
  | TypeIdentity
  | TypeTable
  | TypeDiscrete
  | TypeLinear
  | TypeGamma
  | TypeFractalNoise
  | TypeTurbulence
  | Type String
  deriving (Eq, Show, Generic, Hashable)

-- u1                                             -- TODO

-- u2                                             -- TODO

-- underline-position
newtype UnderlinePosition = UnderlinePosition Number
  deriving (Eq, Show, Generic, Hashable)

-- underline-thickness
newtype UnderlineThickness = UnderlineThickness Number
  deriving (Eq, Show, Generic, Hashable)

-- unicode
newtype Unicode = Unicode String
  deriving (Eq, Show, Generic, Hashable)

-- unicode-bidi
data UnicodeBidi
  = UnicodeBidiNormal
  | UnicodeBidiEmbed
  | UnicodeBidiIsolate
  | UnicodeBidiBidiOverride
  | UnicodeBidiIsolateOverride
  | UnicodeBidiPlaintext
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg UnicodeBidi where
--   defaultSvg = UnicodeBidiNormal

-- unicode-range                                  -- TODO

-- units-per-em
newtype UnitsPerEm = UnitsPerEm Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg UnitsPerEm where
--   defaultSvg = UnitsPerEm $ Num 1000

-- v-alphabetic
newtype VAlphabetic = VAlphabetic Number
  deriving (Eq, Show, Generic, Hashable)

-- v-hanging
newtype VHanging = VHanging Number
  deriving (Eq, Show, Generic, Hashable)

-- v-ideographic
newtype VIdeograph = VIdeograph Number
  deriving (Eq, Show, Generic, Hashable)

-- v-mathematical
newtype VMathematical = VMathematical Number
  deriving (Eq, Show, Generic, Hashable)

-- values                                         -- TODO

-- vector-effect
data VectorEffect
  = VectorEffectNone
  | VectorEffectNonScalingStroke
  | VectorEffectNonScalingSize
  | VectorEffectNonRotation
  | VectorEffectFixedPosition
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg VectorEffect where
--   defaultSvg = VectorEffectNone

-- version
newtype Version = Version Number
  deriving (Eq, Show, Generic, Hashable)

-- vert-adv-y
newtype VertAdvY = VertAdvY Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg VertAdvY where
--   defaultSvg = VertAdvY $ Num 1

-- vert-origin-x
newtype VertOriginX = VertOriginX Number
  deriving (Eq, Show, Generic, Hashable)

-- vert-origin-y
newtype VertOriginY = VertOriginY Number
  deriving (Eq, Show, Generic, Hashable)

-- viewBox        -- Change implementation to (ListOfTS Number)?
data ViewBox = ViewBox (Maybe (Number, Number, Number, Number))
  deriving (Eq, Show, Generic, Hashable)

-- viewTarget
newtype ViewTarget = ViewTarget Name
  deriving (Eq, Show, Generic, Hashable)

-- visibility
data Visibility
  = Visible
  | Hidden
  | Collapse
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg Visibility where
--   defaultSvg = Visible

-- width
data WidthAttr
  = WidthAttrAuto
  | WidthAttr Length
  deriving (Eq, Show, Generic, Hashable)

-- widths
newtype Widths = Widths Number
  deriving (Eq, Show, Generic, Hashable)

-- word-spacing
data WordSpacing
  = WordSpacingNormal
  | WordSpacing Length
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg WordSpacing where
--   defaultSvg = WordSpacingNormal

-- writing-mode
data WritingMode
  = WritingModeHorizontalTb
  | WritingModeVerticalTb
  | WritingModeVerticalLr
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg WritingMode where
--   defaultSvg = WritingModeHorizontalTb

-- x
data XAttr
  = XAttrList (ListOfTs Length)
  | XAttrPercent Percentage
  | XAttr Length
  | XAttrNum Number
  deriving (Eq, Show, Generic, Hashable)

-- x-height
newtype XHeight = XHeight Number
  deriving (Eq, Show, Generic, Hashable)

-- x1
data X1
  = X1Percent Percentage
  | X1 Length
  deriving (Eq, Show, Generic, Hashable)

-- x2
data X2
  = X2Percent Percentage
  | X2 Length
  deriving (Eq, Show, Generic, Hashable)

-- xChannelSelector
data XChannelSelector
  = XChannelR
  | XChannelG
  | XChannelB
  | XChannelA
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg XChannelSelector where
--   defaultSvg = XChannelA

-- xlink:actuate                                  -- Definition is missing.

-- xlink:arcrole                                  -- TODO

-- xlink:href                                     -- TODO

-- xlink:role                                     -- Definition is missing.

-- xlink:show
data XlinkShow
  = XlinkShowNew
  | XlinkShowReplace
  | XlinkShowEmbed
  | XlinkShowOther
  | XlinkShowNone
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg XlinkShow where
--   defaultSvg = XlinkShowReplace

-- xlink:title
newtype XlinkTitle = XlinkTitle Anything
  deriving (Eq, Show, Generic, Hashable)

-- xlink:type
data XlinkType = XlinkTypeSimple
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg XlinkType where
--   defaultSvg = XlinkTypeSimple

-- xml:base                                       -- TODO
--data XmlBase

-- xml:lang
newtype XmlLang = XmlLang Name
 deriving (Eq, Show, Generic, Hashable)

-- xml:space
data XmlSpace
  = XmlSpaceDefault
  | XmlSpacePreserve
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg XmlSpace where
--   defaultSvg = XmlSpaceDefault

-- y
data YAttr
  = YAttrList (ListOfTs Length)
  | YAttrPercent Percentage
  | YAttr Length
  | YAttrNum Number
  deriving (Eq, Show, Generic, Hashable)

-- y1
data Y1
  = Y1Percent Percentage
  | Y1 Length
  deriving (Eq, Show, Generic, Hashable)

-- y2
data Y2
  = Y2Percent Percentage
  | Y2 Length
  deriving (Eq, Show, Generic, Hashable)

-- yChannelSelector
data YChannelSelector
  = YChannelR
  | YChannelG
  | YChannelB
  | YChannelA
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg YChannelSelector where
--   defaultSvg = YChannelA

-- z
newtype ZAttr = ZAttr Number
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg ZAttr where
--   defaultSvg = ZAttr $ Num 1

-- zoomAndPan
data ZoomAndPan
  = ZoomAndPanDisable
  | ZoomAndPanMagnify
  deriving (Eq, Show, Generic, Hashable)

-- instance WithDefaultSvg ZoomAndPan where
--   defaultSvg = ZoomAndPanMagnify


-- Lenses declarations
makeLenses ''PreserveAspectRatio
makeClassy ''CoreAttributes
makeClassy ''StylingAttributes
makeClassy ''ConditionalProcessingAttributes
makeClassy ''XLinkAttributes
makeClassy ''PresentationAttributes
makeClassy ''FilterPrimitiveAttributes
makeClassy ''TransferFunctionAttributes
