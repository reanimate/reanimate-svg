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

-- | Real Point, fully determined and not
-- dependant of the rendering context.
type RPoint = V2 Coord

-- FIXME: Use 'V2 Number' instead of tuple
-- | Possibly context dependant point.
type Point = (Number, Number)

-- | Tell if a path command is absolute (in the current
-- user coordiante) or relative to the previous poitn.
data Origin
  = OriginAbsolute -- ^ Next point in absolute coordinate
  | OriginRelative -- ^ Next point relative to the previous
  deriving (Eq, Show, Generic)


data MeshGradientType
  = GradientBilinear
  | GradientBicubic
  deriving (Eq, Show, Generic)

-- | Define the possible values of various *units attributes
-- used in the definition of the gradients and masks.
data CoordinateUnits
    = CoordUserSpace   -- ^ `userSpaceOnUse` value
    | CoordBoundingBox -- ^ `objectBoundingBox` value
    deriving (Eq, Show, Generic)

-- | This type represent the align information of the
-- preserveAspectRatio SVGattribute
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
  deriving (Eq, Show, Generic)

-- | This type represent the "meet or slice" information
-- of the preserveAspectRatio SVGattribute
data MeetSlice = Meet | Slice
    deriving (Eq, Show, Generic)

-- | Describe how the line should be terminated
-- when stroking them. Describe the values of the
-- `stroke-linecap` attribute.
-- See `_strokeLineCap`
data Cap
  = CapRound -- ^ End with a round (`round` value)
  | CapButt  -- ^ Define straight just at the end (`butt` value)
  | CapSquare -- ^ Straight further of the ends (`square` value)
  deriving (Eq, Show, Generic)

-- | Define the possible values of the `stroke-linejoin`
-- attribute.
-- see `_strokeLineJoin`
data LineJoin
    = JoinMiter -- ^ `miter` value
    | JoinBevel -- ^ `bevel` value
    | JoinRound -- ^ `round` value
    deriving (Eq, Show, Generic)

-- | Describe the different value which can be used
-- in the `fill` or `stroke` attributes.
data Texture
  = ColorRef   PixelRGBA8 -- ^ Direct solid color (#rrggbb, #rgb)
  | ColorNamed String     -- ^ Named color such as "black" or "fuchsia"
  | TextureRef String     -- ^ Link to a complex texture (url(#name))
  | FillNone              -- ^ Equivalent to the `none` value.
  deriving (Eq, Show, Generic)

-- | Describe the possile filling algorithms.
-- Map the values of the `fill-rule` attributes.
data FillRule
    = FillEvenOdd -- ^ Correspond to the `evenodd` value.
    | FillNonZero -- ^ Correspond to the `nonzero` value.
    deriving (Eq, Show, Generic)
