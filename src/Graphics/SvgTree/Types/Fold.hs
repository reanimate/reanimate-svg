module Graphics.SvgTree.Types.Fold where

import           Control.Lens           ((%~), (&), (.~), (^.))
import qualified Data.Foldable          as F
import           Data.List              (inits)
import           Graphics.SvgTree.Types.Internal
import           Graphics.SvgTree.Types.Hashable

-- | Insert element in the first sublist in the list of list.
appNode :: [[a]] -> a -> [[a]]
appNode [] e           = [[e]]
appNode (curr:above) e = (e:curr) : above

-- | Map a tree while propagating context information.
-- The function passed in parameter receive a list
-- representing the the path used to go arrive to the
-- current node.
zipTree :: ([[Tree]] -> Tree) -> Tree -> Tree
zipTree f = dig [] where
  dig prev e = case e ^. treeBranch of
    None -> f $ appNode prev e
    UseTree _ Nothing -> f $ appNode prev e
    UseTree nfo (Just u) ->
      f $ appNode prev $ e & treeBranch .~ UseTree nfo (Just $ dig ([] : appNode prev e) u)
    GroupTree g ->
      f $ appNode prev $ e & treeBranch .~ GroupTree (zipGroup (appNode prev e) g)
    SymbolTree g ->
      f $ appNode prev $ e & treeBranch .~ (SymbolTree . Symbol .
            zipGroup (appNode prev e) $ _groupOfSymbol g)
    DefinitionTree g ->
      f $ appNode prev $ e & treeBranch .~ (DefinitionTree . Definitions .
            zipGroup (appNode prev e) $ _groupOfDefinitions g)
    FilterTree{} -> f $ appNode prev e
    PathTree{} -> f $ appNode prev e
    CircleTree{} -> f $ appNode prev e
    PolyLineTree{} -> f $ appNode prev e
    PolygonTree{} -> f $ appNode prev e
    EllipseTree{} -> f $ appNode prev e
    LineTree{} -> f $ appNode prev e
    RectangleTree{} -> f $ appNode prev e
    TextTree{} -> f $ appNode prev e
    ImageTree{} -> f $ appNode prev e
    MeshGradientTree{} -> f $ appNode prev e
    LinearGradientTree{} -> f $ appNode prev e
    RadialGradientTree{} -> f $ appNode prev e
    PatternTree{} -> f $ appNode prev e
    MarkerTree{} -> f $ appNode prev e
    MaskTree{} -> f $ appNode prev e
    ClipPathTree{} -> f $ appNode prev e
    SvgTree{} -> f $ appNode prev e

  zipGroup prev g = g { _groupChildren = updatedChildren }
    where
      groupChild = _groupChildren g
      updatedChildren =
        [dig (c:prev) child
            | (child, c) <- zip groupChild $ inits groupChild]

-- | Fold all nodes of a SVG tree.
foldTree :: (a -> Tree -> a) -> a -> Tree -> a
foldTree f = go where
  go acc e = case e ^. treeBranch of
    None                 -> f acc e
    UseTree{}            -> f acc e
    PathTree{}           -> f acc e
    CircleTree{}         -> f acc e
    PolyLineTree{}       -> f acc e
    PolygonTree{}        -> f acc e
    EllipseTree{}        -> f acc e
    LineTree{}           -> f acc e
    RectangleTree{}      -> f acc e
    TextTree{}           -> f acc e
    ImageTree{}          -> f acc e
    LinearGradientTree{} -> f acc e
    RadialGradientTree{} -> f acc e
    MeshGradientTree{}   -> f acc e
    PatternTree{}        -> f acc e
    MarkerTree{}         -> f acc e
    MaskTree{}           -> f acc e
    ClipPathTree{}       -> f acc e
    DefinitionTree g     -> foldGroup (_groupOfDefinitions g)
    FilterTree{}         -> f acc e
    GroupTree g          -> foldGroup g
    SymbolTree s         -> foldGroup (_groupOfSymbol s)
    SvgTree{}            -> f acc e
    where
      foldGroup g =
        let subAcc = F.foldl' go acc $ _groupChildren g in
        f subAcc e

-- | Helper function mapping every tree element.
mapTree :: (Tree -> Tree) -> Tree -> Tree
mapTree f = worker where
  worker t = f $ t & treeBranch %~ go
  go e = case e of
    None -> e
    UseTree{}    -> e
    GroupTree g  -> GroupTree $ mapGroup g
    SymbolTree g ->
      SymbolTree . Symbol . mapGroup $ _groupOfSymbol g
    DefinitionTree defs ->
      DefinitionTree . Definitions . mapGroup $ _groupOfDefinitions defs
    FilterTree{} -> e
    PathTree{} -> e
    CircleTree{} -> e
    PolyLineTree{} -> e
    PolygonTree{} -> e
    EllipseTree{} -> e
    LineTree{} -> e
    RectangleTree{} -> e
    TextTree{} -> e
    ImageTree{} -> e
    LinearGradientTree{} -> e
    RadialGradientTree{} -> e
    MeshGradientTree{} -> e
    PatternTree{} -> e
    MarkerTree{} -> e
    MaskTree{} -> e
    ClipPathTree{} -> e
    SvgTree{} -> e

  mapGroup g =
      g { _groupChildren = map worker $ _groupChildren g }
