module Graphics.SvgTree.Types.Constructors where

import Graphics.SvgTree.Types.Internal
import Graphics.SvgTree.Types.Hashable

useTree :: Use -> Tree
useTree u = UseTree u Nothing

groupTree :: Group -> Tree
groupTree = GroupTree

symbolTree :: Group -> Tree
symbolTree = SymbolTree

definitionTree :: Group -> Tree
definitionTree = DefinitionTree

filterTree :: Filter -> Tree
filterTree = FilterTree

pathTree :: Path -> Tree
pathTree = PathTree

circleTree :: Circle -> Tree
circleTree = CircleTree

polyLineTree :: PolyLine -> Tree
polyLineTree = PolyLineTree

polygonTree :: Polygon -> Tree
polygonTree = PolygonTree

ellipseTree :: Ellipse -> Tree
ellipseTree = EllipseTree

lineTree :: Line -> Tree
lineTree = LineTree

rectangleTree :: Rectangle -> Tree
rectangleTree = RectangleTree

textTree :: Maybe TextPath -> Text -> Tree
textTree = TextTree

imageTree :: Image -> Tree
imageTree = ImageTree

linearGradientTree :: LinearGradient -> Tree
linearGradientTree = LinearGradientTree

radialGradientTree :: RadialGradient -> Tree
radialGradientTree = RadialGradientTree

meshGradientTree :: MeshGradient -> Tree
meshGradientTree = MeshGradientTree

patternTree :: Pattern -> Tree
patternTree = PatternTree

markerTree :: Marker -> Tree
markerTree = MarkerTree

maskTree :: Mask -> Tree
maskTree = MaskTree

clipPathTree :: ClipPath -> Tree
clipPathTree = ClipPathTree

svgTree :: Document -> Tree
svgTree = SvgTree
