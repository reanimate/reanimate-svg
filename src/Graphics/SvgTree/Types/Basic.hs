{-# LANGUAGE DeriveGeneric #-}
{-
  Home of basic types without fields.
-}
module Graphics.SvgTree.Types.Basic where

import           Codec.Picture             (PixelRGBA8 (..))
import           GHC.Generics              (Generic)
import           Graphics.SvgTree.CssTypes (Number)
import           Linear                    (V2)

-- | Basic coordinate type.
type Coord = Double

-- | Real Point, fully determined and
-- independent of the rendering context.
type RPoint = V2 Coord

-- FIXME: Use 'V2 Number' instead of tuple
-- | Possibly context dependant point.
type Point = (Number, Number)

-- | Tell if a path command is absolute (in the current
-- user coordiante) or relative to the previous point.
data Origin
  = OriginAbsolute -- ^ Next point in absolute coordinate.
  | OriginRelative -- ^ Next point relative to the previous.
  deriving (Eq, Show, Generic)


data MeshGradientType
  = GradientBilinear
  | GradientBicubic
  deriving (Eq, Show, Generic)

-- | Defines the possible values of various *units SVG attributes
-- used in the definition of the gradients and masks.
data CoordinateUnits
    = CoordUserSpace   -- ^ @userSpaceOnUse@ SVG value.
    | CoordBoundingBox -- ^ @objectBoundingBox@ SVG value.
    deriving (Eq, Show, Generic)

-- | This type represents the align information of the
-- @preserveAspectRatio@ SVG attribute.
data Alignment
  = AlignNone     -- ^ @none@ SVG value.
  | AlignxMinYMin -- ^ @xMinYMin@ SVG value.
  | AlignxMidYMin -- ^ @xMidYMin@ SVG value.
  | AlignxMaxYMin -- ^ @xMaxYMin@ SVG value.
  | AlignxMinYMid -- ^ @xMinYMid@ SVG value.
  | AlignxMidYMid -- ^ @xMidYMid@ SVG value.
  | AlignxMaxYMid -- ^ @xMaxYMid@ SVG value.
  | AlignxMinYMax -- ^ @xMinYMax@ SVG value.
  | AlignxMidYMax -- ^ @xMidYMax@ SVG value.
  | AlignxMaxYMax -- ^ @xMaxYMax@ SVG value.
  deriving (Eq, Show, Generic)

-- | This type represents the "meet or slice" information
-- of the @preserveAspectRatio@ SVG attribute.
data MeetSlice = Meet | Slice
    deriving (Eq, Show, Generic)

-- | Describes how the line should be terminated
-- when stroked. Describes the values of the
-- @stroke-linecap@ SVG attribute.
-- See 'Graphics.SvgTree.Types._strokeLineCap'.
data Cap
  = CapRound -- ^ End with a round (@round@ SVG value).
  | CapButt  -- ^ Define straight just at the end (@butt@ SVG value).
  | CapSquare -- ^ Straight further of the ends (@square@ SVG value).
  deriving (Eq, Show, Generic)

-- | Defines the possible values of the @stroke-linejoin@ SVG
-- attribute.
-- See 'Graphics.SvgTree.Types._strokeLineJoin'.
data LineJoin
    = JoinMiter -- ^ @miter@ SVG value.
    | JoinBevel -- ^ @bevel@ SVG value.
    | JoinRound -- ^ @round@ SVG value.
    deriving (Eq, Show, Generic)

-- | Describes the different values which can be used
-- in the @fill@ or @stroke@ SVG attributes.
data Texture
  = ColorRef   PixelRGBA8 -- ^ Direct solid color (@#rrggbb@, @#rgb@).
  | TextureRef String     -- ^ Link to a complex texture (@url(\<string\>)@
                          -- SVG value).
  | FillNone              -- ^ Equivalent to the @none@ SVG value.
  deriving (Eq, Show, Generic)

-- | Describe the possible filling algorithms.
-- Map the values of the @fill-rule@ SVG attributes.
data FillRule
    = FillEvenOdd -- ^ Corresponds to the @evenodd@ SVG value.
    | FillNonZero -- ^ Corresponds to the @nonzero@ SVG value.
    deriving (Eq, Show, Generic)
