{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans -O0 #-}
module Graphics.SvgTree.Types.Hashable where

import Data.Hashable (Hashable)
import Graphics.SvgTree.Types

deriving instance Hashable DrawAttributes
deriving instance Hashable Pattern
deriving instance Hashable Element
deriving instance Hashable ClipPath
deriving instance Hashable Mask
deriving instance Hashable CoordinateUnits
deriving instance Hashable Tree
deriving instance Hashable a => Hashable (Group a)
deriving instance Hashable a => Hashable (Symbol a)
deriving instance Hashable a => Hashable (Definitions a)
deriving instance Hashable PreserveAspectRatio
deriving instance Hashable Alignment
deriving instance Hashable MeetSlice
deriving instance Hashable LinearGradient
deriving instance Hashable Spread
deriving instance Hashable Transformation
deriving instance Hashable GradientStop
deriving instance Hashable GradientPathCommand
deriving instance Hashable Origin
deriving instance Hashable Use
deriving instance Hashable Filter
deriving instance Hashable FilterAttributes
deriving instance Hashable FilterElement
deriving instance Hashable ColorMatrix
deriving instance Hashable FilterSource
deriving instance Hashable ColorMatrixType
deriving instance Hashable Composite
deriving instance Hashable CompositeOperator
deriving instance Hashable DisplacementMap
deriving instance Hashable ChannelSelector
deriving instance Hashable GaussianBlur
deriving instance Hashable EdgeMode
deriving instance Hashable Turbulence
deriving instance Hashable StitchTiles
deriving instance Hashable TurbulenceType
deriving instance Hashable Path
deriving instance Hashable PathCommand
deriving instance Hashable Circle
deriving instance Hashable PolyLine
deriving instance Hashable Polygon
deriving instance Hashable Ellipse
deriving instance Hashable Line
deriving instance Hashable Rectangle
deriving instance Hashable TextPath
deriving instance Hashable TextPathMethod
deriving instance Hashable TextPathSpacing
deriving instance Hashable Text
deriving instance Hashable TextAdjust
deriving instance Hashable TextSpan
deriving instance Hashable TextInfo
deriving instance Hashable TextSpanContent
deriving instance Hashable Image
deriving instance Hashable RadialGradient
deriving instance Hashable MeshGradient
deriving instance Hashable MeshGradientType
deriving instance Hashable MeshGradientRow
deriving instance Hashable MeshGradientPatch
deriving instance Hashable Marker
deriving instance Hashable MarkerOrientation
deriving instance Hashable MarkerUnit
deriving instance Hashable Overflow
deriving instance Hashable Document
deriving instance Hashable Texture
deriving instance Hashable Cap
deriving instance Hashable LineJoin
deriving instance Hashable FillRule
deriving instance Hashable ElementRef
deriving instance Hashable FontStyle
deriving instance Hashable TextAnchor
