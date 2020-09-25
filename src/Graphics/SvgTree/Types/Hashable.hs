{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans -O0 #-}

module Graphics.SvgTree.Types.Hashable where

import Codec.Picture (PixelRGBA8 (..))
import Control.Lens
import Data.Hashable
import Data.Monoid
import GHC.Generics (Generic)
import Graphics.SvgTree.Types.Internal

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

deriving instance Hashable Group

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

deriving instance Hashable Blend
deriving instance Hashable BlendMode

deriving instance Hashable Flood

deriving instance Hashable Tile

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

treeBranch :: Lens' Tree TreeBranch
treeBranch = lens _treeBranch $ const Tree

instance WithDefaultSvg Tree where
  defaultSvg = Tree NoNode

unpack :: Tree -> TreeBranch
unpack t = (_treeBranch t)

pattern Tree :: TreeBranch -> Tree
pattern Tree branch <-
  CachedTree {_treeBranch = branch}
  where
    Tree branch =
      CachedTree
        { _treeBranch = branch,
          _treeHash = hash branch
        }

pattern GroupTree :: Group -> Tree
pattern GroupTree g = Tree (GroupNode g)

pattern SymbolTree :: Group -> Tree
pattern SymbolTree g = Tree (SymbolNode g)

pattern DefinitionTree :: Group -> Tree
pattern DefinitionTree g = Tree (DefinitionNode g)

pattern None :: Tree
pattern None = Tree NoNode

pattern UseTree :: Use -> Maybe Tree -> Tree
pattern UseTree info sub = Tree (UseNode info sub)

pattern FilterTree :: Filter -> Tree
pattern FilterTree f = Tree (FilterNode f)

pattern PathTree :: Path -> Tree
pattern PathTree f = Tree (PathNode f)

pattern CircleTree :: Circle -> Tree
pattern CircleTree f = Tree (CircleNode f)

pattern PolyLineTree :: PolyLine -> Tree
pattern PolyLineTree f = Tree (PolyLineNode f)

pattern PolygonTree :: Polygon -> Tree
pattern PolygonTree f = Tree (PolygonNode f)

pattern EllipseTree :: Ellipse -> Tree
pattern EllipseTree f = Tree (EllipseNode f)

pattern LineTree :: Line -> Tree
pattern LineTree f = Tree (LineNode f)

pattern RectangleTree :: Rectangle -> Tree
pattern RectangleTree f = Tree (RectangleNode f)

pattern TextTree :: Maybe TextPath -> Text -> Tree
pattern TextTree p t = Tree (TextNode p t)

pattern ImageTree :: Image -> Tree
pattern ImageTree n = Tree (ImageNode n)

pattern LinearGradientTree :: LinearGradient -> Tree
pattern LinearGradientTree n = Tree (LinearGradientNode n)

pattern RadialGradientTree :: RadialGradient -> Tree
pattern RadialGradientTree n = Tree (RadialGradientNode n)

pattern MeshGradientTree :: MeshGradient -> Tree
pattern MeshGradientTree n = Tree (MeshGradientNode n)

pattern PatternTree :: Pattern -> Tree
pattern PatternTree n = Tree (PatternNode n)

pattern MarkerTree :: Marker -> Tree
pattern MarkerTree n = Tree (MarkerNode n)

pattern MaskTree :: Mask -> Tree
pattern MaskTree n = Tree (MaskNode n)

pattern ClipPathTree :: ClipPath -> Tree
pattern ClipPathTree n = Tree (ClipPathNode n)

pattern SvgTree :: Document -> Tree
pattern SvgTree n = Tree (SvgNode n)
