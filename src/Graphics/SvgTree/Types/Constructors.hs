module Graphics.SvgTree.Types.Constructors where

import Graphics.SvgTree.Types.Internal
import Graphics.SvgTree.Types.Hashable

groupTree :: Group -> Tree
groupTree = Tree . GroupTree

symbolTree :: Symbol -> Tree
symbolTree = Tree . SymbolTree

definitionTree :: Definitions -> Tree
definitionTree = Tree . DefinitionTree

filterTree :: Filter -> Tree
filterTree = Tree . FilterTree

pathTree :: Path -> Tree
pathTree = Tree . PathTree

circleTree :: Circle -> Tree
circleTree = Tree . CircleTree

polyLineTree :: PolyLine -> Tree
polyLineTree = Tree . PolyLineTree

polygonTree :: Polygon -> Tree
polygonTree = Tree . PolygonTree

ellipseTree :: Ellipse -> Tree
ellipseTree = Tree . EllipseTree

lineTree :: Line -> Tree
lineTree = Tree . LineTree

rectangleTree :: Rectangle -> Tree
rectangleTree = Tree . RectangleTree

textTree :: Maybe TextPath -> Text -> Tree
textTree a b = Tree $ TextTree a b

imageTree :: Image -> Tree
imageTree = Tree . ImageTree

linearGradientTree :: LinearGradient -> Tree
linearGradientTree = Tree . LinearGradientTree

radialGradientTree :: RadialGradient -> Tree
radialGradientTree = Tree . RadialGradientTree

meshGradientTree :: MeshGradient -> Tree
meshGradientTree = Tree . MeshGradientTree

patternTree :: Pattern -> Tree
patternTree = Tree . PatternTree

markerTree :: Marker -> Tree
markerTree = Tree . MarkerTree

maskTree :: Mask -> Tree
maskTree = Tree . MaskTree

clipPathTree :: ClipPath -> Tree
clipPathTree = Tree . ClipPathTree

svgTree :: Document -> Tree
svgTree = Tree . SvgTree
