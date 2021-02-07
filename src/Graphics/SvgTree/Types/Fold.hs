module Graphics.SvgTree.Types.Fold where

import           Control.Lens           ((%~), (&), (^.))
import qualified Data.Foldable          as F
import           Data.List              (inits)
import           Graphics.SvgTree.Types.Internal
import           Graphics.SvgTree.Types.Hashable

-- | Insert element in the first sublist in the list of list.
appNode :: [[a]] -> a -> [[a]]
appNode [] e           = [[e]]
appNode (curr:above) e = (e:curr) : above

-- | Map a tree while propagating context information.
-- The function passed in parameter receives a list
-- representing the path used to go arrive to the
-- current node.
zipTree :: ([[Tree]] -> Tree) -> Tree -> Tree
zipTree f = dig [] where
  dig prev e = case e ^. treeBranch of
    NoNode -> f $ appNode prev e
    UseNode _ Nothing -> f $ appNode prev e
    UseNode nfo (Just u) ->
      f $ appNode prev $ UseTree nfo (Just $ dig ([] : appNode prev e) u)
    GroupNode g ->
      f $ appNode prev $ GroupTree $ zipGroup (appNode prev e) g
    SymbolNode g ->
      f $ appNode prev $ SymbolTree $ zipGroup (appNode prev e) g
    DefinitionNode g ->
      f $ appNode prev $ DefinitionTree $ zipGroup (appNode prev e) g
    FilterNode{} -> f $ appNode prev e
    PathNode{} -> f $ appNode prev e
    CircleNode{} -> f $ appNode prev e
    PolyLineNode{} -> f $ appNode prev e
    PolygonNode{} -> f $ appNode prev e
    EllipseNode{} -> f $ appNode prev e
    LineNode{} -> f $ appNode prev e
    RectangleNode{} -> f $ appNode prev e
    TextNode{} -> f $ appNode prev e
    ImageNode{} -> f $ appNode prev e
    MeshGradientNode{} -> f $ appNode prev e
    LinearGradientNode{} -> f $ appNode prev e
    RadialGradientNode{} -> f $ appNode prev e
    PatternNode{} -> f $ appNode prev e
    MarkerNode{} -> f $ appNode prev e
    MaskNode{} -> f $ appNode prev e
    ClipPathNode{} -> f $ appNode prev e
    SvgNode{} -> f $ appNode prev e

  zipGroup prev g = g { _groupChildren = updatedChildren }
    where
      groupChild = _groupChildren g
      updatedChildren =
        [dig (c:prev) child
            | (child, c) <- zip groupChild $ inits groupChild]

-- | Fold all nodes of a SVG tree.
foldTree :: (a -> Tree -> a) -> a -> Tree -> a
foldTree f = go where
  go acc e = case e of
    DefinitionTree g   -> foldGroup g
    GroupTree g        -> foldGroup g
    SymbolTree g       -> foldGroup g
    _                  -> f acc e
    where
      foldGroup g =
        let subAcc = F.foldl' go acc $ _groupChildren g in
        f subAcc e

-- | Helper function mapping every tree element.
mapTree :: (Tree -> Tree) -> Tree -> Tree
mapTree f = worker where
  worker t = f $ t & treeBranch %~ go
  go e = case e of
    NoNode -> e
    UseNode{}    -> e
    GroupNode g  -> GroupNode $ mapGroup g
    SymbolNode g ->
      SymbolNode $ mapGroup g
    DefinitionNode g ->
      DefinitionNode $ mapGroup g
    FilterNode{} -> e
    PathNode{} -> e
    CircleNode{} -> e
    PolyLineNode{} -> e
    PolygonNode{} -> e
    EllipseNode{} -> e
    LineNode{} -> e
    RectangleNode{} -> e
    TextNode{} -> e
    ImageNode{} -> e
    LinearGradientNode{} -> e
    RadialGradientNode{} -> e
    MeshGradientNode{} -> e
    PatternNode{} -> e
    MarkerNode{} -> e
    MaskNode{} -> e
    ClipPathNode{} -> e
    SvgNode{} -> e

  mapGroup g =
      g { _groupChildren = map worker $ _groupChildren g }

mapBranch :: (TreeBranch -> TreeBranch) -> Tree -> Tree
mapBranch f = mapTree (treeBranch %~ f)
