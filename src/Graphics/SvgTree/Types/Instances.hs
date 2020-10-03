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

instance HasDrawAttributes Blend where
  drawAttributes = blendDrawAttributes

instance HasDrawAttributes ConvolveMatrix where
  drawAttributes = convolveMatrixDrawAttributes

instance HasDrawAttributes Morphology where
  drawAttributes = morphologyDrawAttributes

instance HasDrawAttributes SpecularLighting where
  drawAttributes = specLightingDrawAttributes

instance HasDrawAttributes DropShadow where
  drawAttributes = dropShadowDrawAttributes

instance HasDrawAttributes DiffuseLighting where
  drawAttributes = diffuseLightingDrawAttributes

instance HasDrawAttributes ComponentTransfer where
  drawAttributes = compTransferDrawAttributes

instance HasDrawAttributes FuncA where
  drawAttributes = funcADrawAttributes
instance HasDrawAttributes FuncR where
  drawAttributes = funcRDrawAttributes
instance HasDrawAttributes FuncG where
  drawAttributes = funcGDrawAttributes
instance HasDrawAttributes FuncB where
  drawAttributes = funcBDrawAttributes

instance HasDrawAttributes Flood where
  drawAttributes = floodDrawAttributes

instance HasDrawAttributes Tile where
  drawAttributes = tileDrawAttributes

instance HasDrawAttributes Offset where
  drawAttributes = offsetDrawAttributes

instance HasDrawAttributes Merge where
  drawAttributes = mergeDrawAttributes

instance HasDrawAttributes ImageF where
  drawAttributes = imageFDrawAttributes

instance HasDrawAttributes MergeNode where
  drawAttributes = mergeNodeDrawAttributes

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


instance HasFilterAttributes Filter where
  filterAttributes = filterSelfAttributes

instance HasFilterAttributes Blend where
  filterAttributes = blendFilterAttr

instance HasFilterAttributes ConvolveMatrix where
  filterAttributes = convolveMatrixFilterAttr

instance HasFilterAttributes Morphology where
  filterAttributes = morphologyFilterAttr

instance HasFilterAttributes SpecularLighting where
  filterAttributes = specLightingFilterAttr

instance HasFilterAttributes DropShadow where
  filterAttributes = dropShadowFilterAttr

instance HasFilterAttributes DiffuseLighting where
  filterAttributes = diffuseLightingFilterAttr

instance HasFilterAttributes Flood where
  filterAttributes = floodFilterAttr

instance HasFilterAttributes Tile where
  filterAttributes = tileFilterAttr

instance HasFilterAttributes Offset where
  filterAttributes = offsetFilterAttr

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

instance HasFilterAttributes Merge where
  filterAttributes = mergeFilterAttributes

instance HasFilterAttributes ImageF where
  filterAttributes = imageFFilterAttr

instance HasFilterAttributes ComponentTransfer where
  filterAttributes = compTransferFilterAttr



instance HasFilterAttributes FilterElement where
  filterAttributes = lens getter setter
    where
      getter fe = case fe of
        FEBlend b -> b ^. filterAttributes
        FEColorMatrix m -> m ^. filterAttributes
        FEComponentTransfer c -> c ^. filterAttributes
        FEComposite c -> c ^. filterAttributes
        FEConvolveMatrix c -> c ^. filterAttributes
        FEDiffuseLighting d -> d ^. filterAttributes
        FEDisplacementMap d -> d ^. filterAttributes
        FEDropShadow d -> d ^. filterAttributes
        FEFlood f -> f ^. filterAttributes
        FEFuncA _ -> defaultSvg --FuncA has no filter attributes
        FEFuncR _ -> defaultSvg --FuncR has no filter attributes
        FEFuncG _ -> defaultSvg --FuncG has no filter attributes
        FEFuncB _ -> defaultSvg --FuncB has no filter attributes
        FEGaussianBlur g -> g ^. filterAttributes
        FEImage i -> i ^. filterAttributes
        FEMergeNode _ -> defaultSvg --MergeNode has no filterAttributes!
        FEMerge m -> m ^. filterAttributes
        FEMorphology m -> m ^. filterAttributes
        FEOffset o -> o ^. filterAttributes
        FESpecularLighting s -> s ^. filterAttributes
        FETile t -> t ^. filterAttributes
        FETurbulence t -> t ^. filterAttributes
        FENone -> defaultSvg
      setter fe attr = case fe of
        FEBlend b -> FEBlend $ b & filterAttributes .~ attr
        FEColorMatrix m -> FEColorMatrix $ m & filterAttributes .~ attr
        FEComponentTransfer c -> FEComponentTransfer $ c & filterAttributes .~ attr
        FEComposite c -> FEComposite $ c & filterAttributes .~ attr
        FEConvolveMatrix c -> FEConvolveMatrix $ c & filterAttributes .~ attr
        FEDiffuseLighting d -> FEDiffuseLighting $ d & filterAttributes .~ attr
        FEDisplacementMap d -> FEDisplacementMap $ d & filterAttributes .~ attr
        FEDropShadow d -> FEDropShadow $ d & filterAttributes .~ attr
        FEFlood f -> FEFlood $ f & filterAttributes .~ attr
        FEFuncA _ -> fe --FuncA has no filter atributes
        FEFuncR _ -> fe --FuncR has no filter atributes
        FEFuncG _ -> fe --FuncG has no filter atributes
        FEFuncB _ -> fe --FuncB has no filter atributes
        FEGaussianBlur g -> FEGaussianBlur $ g & filterAttributes .~ attr
        FEImage i -> FEImage $ i & filterAttributes .~ attr
        FEMerge m -> FEMerge $ m & filterAttributes .~ attr
        FEMergeNode _ -> fe --MergeNode has no filterAttributes!
        FEMorphology m -> FEMorphology $ m & filterAttributes .~ attr
        FEOffset o -> FEOffset $ o & filterAttributes .~ attr
        FESpecularLighting s -> FESpecularLighting $ s & filterAttributes .~ attr
        FETile t -> FETile $ t & filterAttributes .~ attr
        FETurbulence t -> FETurbulence $ t & filterAttributes .~ attr
        FENone -> fe

instance HasDrawAttributes TreeBranch where
  drawAttributes = lens getter setter
    where
      getter b = case b of
        NoNode -> defaultSvg
        UseNode use _subNode -> use ^. drawAttributes
        GroupNode t -> t ^. drawAttributes
        SymbolNode t -> t ^. drawAttributes
        DefinitionNode t -> t ^. drawAttributes
        FilterNode t -> t ^. drawAttributes
        PathNode t -> t ^. drawAttributes
        CircleNode t -> t ^. drawAttributes
        PolyLineNode t -> t ^. drawAttributes
        PolygonNode t -> t ^. drawAttributes
        EllipseNode t -> t ^. drawAttributes
        LineNode t -> t ^. drawAttributes
        RectangleNode t -> t ^. drawAttributes
        TextNode _ t -> t ^. drawAttributes
        ImageNode t -> t ^. drawAttributes
        LinearGradientNode t -> t ^. drawAttributes
        RadialGradientNode t -> t ^. drawAttributes
        MeshGradientNode t -> t ^. drawAttributes
        PatternNode t -> t ^. drawAttributes
        MarkerNode t -> t ^. drawAttributes
        MaskNode t -> t ^. drawAttributes
        ClipPathNode t -> t ^. drawAttributes
        SvgNode{} -> defaultSvg
      setter b attr = case b of
        NoNode -> b
        UseNode use subNode -> UseNode (use & drawAttributes .~ attr) subNode
        GroupNode t -> GroupNode $ t & drawAttributes .~ attr
        SymbolNode t -> SymbolNode $ t & drawAttributes .~ attr
        DefinitionNode t -> DefinitionNode $ t & drawAttributes .~ attr
        FilterNode t -> FilterNode $ t & drawAttributes .~ attr
        PathNode t -> PathNode $ t & drawAttributes .~ attr
        CircleNode t -> CircleNode $ t & drawAttributes .~ attr
        PolyLineNode t -> PolyLineNode $ t & drawAttributes .~ attr
        PolygonNode t -> PolygonNode $ t & drawAttributes .~ attr
        EllipseNode t -> EllipseNode $ t & drawAttributes .~ attr
        LineNode t -> LineNode $ t & drawAttributes .~ attr
        RectangleNode t -> RectangleNode $ t & drawAttributes .~ attr
        TextNode path t -> TextNode path $ t & drawAttributes .~ attr
        ImageNode t -> ImageNode $ t & drawAttributes .~ attr
        LinearGradientNode t -> LinearGradientNode $ t & drawAttributes .~ attr
        RadialGradientNode t -> RadialGradientNode $ t & drawAttributes .~ attr
        MeshGradientNode t -> MeshGradientNode $ t & drawAttributes .~ attr
        PatternNode t -> PatternNode $ t & drawAttributes .~ attr
        MarkerNode t -> MarkerNode $ t & drawAttributes .~ attr
        MaskNode t -> MaskNode $ t & drawAttributes .~ attr
        ClipPathNode t -> ClipPathNode $ t & drawAttributes .~ attr
        SvgNode {} -> GroupNode $ defaultSvg & groupChildren .~ [Tree b] & drawAttributes .~ attr
