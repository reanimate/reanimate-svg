{-# OPTIONS_GHC -Wno-orphans #-}
module Graphics.SvgTree.Types.Instances where

import Control.Lens ((&), (.~), (^.), lens, view)
import qualified Data.Text as T
import Graphics.SvgTree.CssTypes (CssMatcheable (..))
import Graphics.SvgTree.Types.Hashable
import Graphics.SvgTree.Types.Internal

instance CssMatcheable Tree where
  cssAttribOf _ _ = Nothing
  cssClassOf = view (drawAttributes . attrClass)
  cssIdOf = fmap T.pack . view (drawAttributes . attrId)
  cssNameOf = nameOfTree

instance HasDrawAttributes Tree where
  drawAttributes = treeBranch . drawAttributes

instance HasDrawAttributes Filter where
  drawAttributes = filterDrawAttributes

instance HasDrawAttributes Use where
  drawAttributes = useDrawAttributes

instance HasDrawAttributes Group where
  drawAttributes = groupDrawAttributes

instance HasDrawAttributes Definitions where
  drawAttributes = group . drawAttributes

instance HasDrawAttributes Symbol where
  drawAttributes = group . drawAttributes

instance HasDrawAttributes Rectangle where
  drawAttributes = rectangleDrawAttributes

instance HasDrawAttributes Line where
  drawAttributes = lineDrawAttributes

instance HasDrawAttributes Ellipse where
  drawAttributes = ellipseDrawAttributes

instance HasDrawAttributes Polygon where
  drawAttributes = polygonDrawAttributes

instance HasDrawAttributes PolyLine where
  drawAttributes = polyLineDrawAttributes

instance HasDrawAttributes Circle where
  drawAttributes = circleDrawAttributes

instance HasDrawAttributes Path where
  drawAttributes = pathDrawAttributes

instance HasDrawAttributes ClipPath where
  drawAttributes = clipPathDrawAttributes

instance HasDrawAttributes Mask where
  drawAttributes = maskDrawAttributes

instance HasDrawAttributes Marker where
  drawAttributes = markerDrawAttributes

instance HasDrawAttributes Image where
  drawAttributes = imageDrawAttributes

instance HasDrawAttributes Pattern where
  drawAttributes = patternDrawAttributes

instance HasDrawAttributes MeshGradient where
  drawAttributes = meshGradientDrawAttributes

instance HasDrawAttributes RadialGradient where
  drawAttributes = radialGradientDrawAttributes

instance HasDrawAttributes LinearGradient where
  drawAttributes = linearGradientDrawAttributes

instance HasDrawAttributes Composite where
  drawAttributes = compositeDrawAttributes

instance HasDrawAttributes ColorMatrix where
  drawAttributes = colorMatrixDrawAttributes

instance HasDrawAttributes GaussianBlur where
  drawAttributes = gaussianBlurDrawAttributes

instance HasDrawAttributes Turbulence where
  drawAttributes = turbulenceDrawAttributes

instance HasDrawAttributes DisplacementMap where
  drawAttributes = displacementMapDrawAttributes

instance HasDrawAttributes Text where
  drawAttributes = textRoot . spanDrawAttributes

instance HasGroup Definitions where
  group = groupOfDefinitions

instance HasGroup Symbol where
  group = groupOfSymbol

instance HasFilterAttributes Filter where
  filterAttributes = filterSelfAttributes

instance HasFilterAttributes Composite where
  filterAttributes = compositeFilterAttr

instance HasFilterAttributes ColorMatrix where
  filterAttributes = colorMatrixFilterAttr

instance HasFilterAttributes GaussianBlur where
  filterAttributes = gaussianBlurFilterAttr

instance HasFilterAttributes Turbulence where
  filterAttributes = turbulenceFilterAttr

instance HasFilterAttributes DisplacementMap where
  filterAttributes = displacementMapFilterAttr

instance HasFilterAttributes FilterElement where
  filterAttributes = lens getter setter
    where
      getter fe = case fe of
        FEBlend -> defaultSvg
        FEColorMatrix m -> m ^. filterAttributes
        FEComponentTransfer -> defaultSvg
        FEComposite c -> c ^. filterAttributes
        FEConvolveMatrix -> defaultSvg
        FEDiffuseLighting -> defaultSvg
        FEDisplacementMap d -> d ^. filterAttributes
        FEDropShadow -> defaultSvg
        FEFlood -> defaultSvg
        FEFuncA -> defaultSvg
        FEFuncB -> defaultSvg
        FEFuncG -> defaultSvg
        FEFuncR -> defaultSvg
        FEGaussianBlur g -> g ^. filterAttributes
        FEImage -> defaultSvg
        FEMerge -> defaultSvg
        FEMergeNode -> defaultSvg
        FEMorphology -> defaultSvg
        FEOffset -> defaultSvg
        FESpecularLighting -> defaultSvg
        FETile -> defaultSvg
        FETurbulence t -> t ^. filterAttributes
        FENone -> defaultSvg
      setter fe attr = case fe of
        FEBlend -> fe
        FEColorMatrix m -> FEColorMatrix $ m & filterAttributes .~ attr
        FEComponentTransfer -> fe
        FEComposite c -> FEComposite $ c & filterAttributes .~ attr
        FEConvolveMatrix -> fe
        FEDiffuseLighting -> fe
        FEDisplacementMap d -> FEDisplacementMap $ d & filterAttributes .~ attr
        FEDropShadow -> fe
        FEFlood -> fe
        FEFuncA -> fe
        FEFuncB -> fe
        FEFuncG -> fe
        FEFuncR -> fe
        FEGaussianBlur g -> FEGaussianBlur $ g & filterAttributes .~ attr
        FEImage -> fe
        FEMerge -> fe
        FEMergeNode -> fe
        FEMorphology -> fe
        FEOffset -> fe
        FESpecularLighting -> fe
        FETile -> fe
        FETurbulence t -> FETurbulence $ t & filterAttributes .~ attr
        FENone -> fe

instance HasDrawAttributes TreeBranch where
  drawAttributes = lens getter setter
    where
      getter b = case b of
        None -> defaultSvg
        UseTree use _subTree -> use ^. drawAttributes
        GroupTree t -> t ^. drawAttributes
        SymbolTree t -> t ^. drawAttributes
        DefinitionTree t -> t ^. drawAttributes
        FilterTree t -> t ^. drawAttributes
        PathTree t -> t ^. drawAttributes
        CircleTree t -> t ^. drawAttributes
        PolyLineTree t -> t ^. drawAttributes
        PolygonTree t -> t ^. drawAttributes
        EllipseTree t -> t ^. drawAttributes
        LineTree t -> t ^. drawAttributes
        RectangleTree t -> t ^. drawAttributes
        TextTree _ t -> t ^. drawAttributes
        ImageTree t -> t ^. drawAttributes
        LinearGradientTree t -> t ^. drawAttributes
        RadialGradientTree t -> t ^. drawAttributes
        MeshGradientTree t -> t ^. drawAttributes
        PatternTree t -> t ^. drawAttributes
        MarkerTree t -> t ^. drawAttributes
        MaskTree t -> t ^. drawAttributes
        ClipPathTree t -> t ^. drawAttributes
        SvgTree{} -> defaultSvg
      setter b attr = case b of
        None -> b
        UseTree use subTree -> UseTree (use & drawAttributes .~ attr) subTree
        GroupTree t -> GroupTree $ t & drawAttributes .~ attr
        SymbolTree t -> SymbolTree $ t & drawAttributes .~ attr
        DefinitionTree t -> DefinitionTree $ t & drawAttributes .~ attr
        FilterTree t -> FilterTree $ t & drawAttributes .~ attr
        PathTree t -> PathTree $ t & drawAttributes .~ attr
        CircleTree t -> CircleTree $ t & drawAttributes .~ attr
        PolyLineTree t -> PolyLineTree $ t & drawAttributes .~ attr
        PolygonTree t -> PolygonTree $ t & drawAttributes .~ attr
        EllipseTree t -> EllipseTree $ t & drawAttributes .~ attr
        LineTree t -> LineTree $ t & drawAttributes .~ attr
        RectangleTree t -> RectangleTree $ t & drawAttributes .~ attr
        TextTree path t -> TextTree path $ t & drawAttributes .~ attr
        ImageTree t -> ImageTree $ t & drawAttributes .~ attr
        LinearGradientTree t -> LinearGradientTree $ t & drawAttributes .~ attr
        RadialGradientTree t -> RadialGradientTree $ t & drawAttributes .~ attr
        MeshGradientTree t -> MeshGradientTree $ t & drawAttributes .~ attr
        PatternTree t -> PatternTree $ t & drawAttributes .~ attr
        MarkerTree t -> MarkerTree $ t & drawAttributes .~ attr
        MaskTree t -> MaskTree $ t & drawAttributes .~ attr
        ClipPathTree t -> ClipPathTree $ t & drawAttributes .~ attr
        SvgTree {} -> GroupTree $ defaultSvg & groupChildren .~ [Tree b] & drawAttributes .~ attr
