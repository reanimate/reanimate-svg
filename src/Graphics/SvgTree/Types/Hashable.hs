{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans -O0 #-}
module Graphics.SvgTree.Types.Hashable where

import           Control.Lens                 (Lens', lens, view)
import           Codec.Picture          (PixelRGBA8 (..))
import           Data.Hashable          
import           Data.Monoid
import           GHC.Generics           (Generic)
import           Graphics.SvgTree.Types.Internal
import           Graphics.SvgTree.CssTypes    (CssMatcheable (..))
import qualified Data.Text                    as T

-- Orphan instances :(
instance Hashable a => Hashable (Last a)

deriving instance Generic PixelRGBA8
instance Hashable PixelRGBA8

deriving instance Hashable DrawAttributes
deriving instance Hashable Pattern
deriving instance Hashable Element
deriving instance Hashable ClipPath
deriving instance Hashable Mask
deriving instance Hashable CoordinateUnits
deriving instance Hashable TreeBranch
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

instance Hashable Tree where
  hashWithSalt s = hashWithSalt s . _treeHash


updTreeHash :: Tree -> Tree
updTreeHash t = t
  { _treeHash = hash (_treeDrawAttributes t, _treeBranch t) }

treeBranch :: Lens' Tree TreeBranch
treeBranch = lens _treeBranch $ \obj val ->
  updTreeHash $ obj { _treeBranch = val }

treeDrawAttributes :: Lens' Tree DrawAttributes
treeDrawAttributes = lens _treeDrawAttributes $ \obj val ->
  updTreeHash $ obj { _treeDrawAttributes = val }

instance HasDrawAttributes Tree where
  drawAttributes = treeDrawAttributes

instance CssMatcheable Tree where
  cssAttribOf _ _ = Nothing
  cssClassOf = view (drawAttributes . attrClass)
  cssIdOf = fmap T.pack . view (drawAttributes . attrId)
  cssNameOf = nameOfTree
