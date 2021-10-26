{-# LANGUAGE PatternSynonyms    #-}
-- | This module define all the types used in the definition
-- of an SVG scene.
--
-- Most of the types are lensified.
module Graphics.SvgTree.Types
    ( -- * Basic building types
      Coord
    , Origin( .. )
    , Point
    , RPoint
    , PathCommand( .. )
    , Transformation( .. )
    , ElementRef( .. )
    , CoordinateUnits( .. )

      -- ** Building helpers
    , serializeNumber
    , serializeTransformation
    , serializeTransformations

      -- * Drawing control types
    , Cap( .. )
    , LineJoin( .. )
    , Tree
    , pattern Tree
    , pattern None
    , treeBranch
    , TreeBranch(..)
    , Number( .. )
    , Spread( .. )
    , Texture( .. )
    , Element( .. )
    , FillRule( .. )
    , FontStyle( .. )
    , Dpi

    , WithDefaultSvg( .. )

      -- * Main type
    , Document( .. )
    , pattern SvgTree
    , svgTree
    ,documentViewBox,
    documentWidth,
    documentHeight,
    documentElements,
    documentDescription,
    documentLocation,
    documentAspectRatio,
    documentSize

      -- * Drawing attributes
    , DrawAttributes( .. )
    , HasDrawAttributes( .. )

      -- * Filters
    , FilterElement(..)
    , FilterAttributes(..)
    , HasFilterAttributes(..)
    , FilterSource(..)

    , Blend (..)
    , BlendMode (..)
    , blendDrawAttributes
    , blendFilterAttr
    , blendIn
    , blendIn2
    , blendMode

    , ConvolveMatrix (..)
    , convolveMatrixDrawAttributes
    , convolveMatrixFilterAttr
    , convolveMatrixIn
    , convolveMatrixOrder
    , convolveMatrixKernelMatrix
    , convolveMatrixDivisor
    , convolveMatrixBias
    , convolveMatrixTargetX
    , convolveMatrixTargetY
    , convolveMatrixEdgeMode
    , convolveMatrixKernelUnitLength
    , convolveMatrixPreserveAlpha

    , Morphology (..)
    , OperatorType (..)
    , NumberOptionalNumber (..)
    , morphologyDrawAttributes
    , morphologyFilterAttr
    , morphologyIn
    , morphologyOperator
    , morphologyRadius

    , SpecularLighting (..)
    , specLightingDrawAttributes
    , specLightingFilterAttr
    , specLightingIn
    , specLightingSurfaceScale
    , specLightingSpecularConst
    , specLightingSpecularExp
    , specLightingKernelUnitLength

    , DiffuseLighting
    , diffuseLightingDrawAttributes
    , diffuseLightingFilterAttr
    , diffuseLightingIn
    , diffuseLightingSurfaceScale
    , diffuseLightingDiffuseConst
    , diffuseLightingKernelUnitLength

    , DropShadow (..)
    , dropShadowDrawAttributes
    , dropShadowFilterAttr
    , dropShadowDx
    , dropShadowDy
    , dropShadowStdDeviation

    , Flood (..)
    , floodDrawAttributes
    , floodFilterAttr
    , floodColor
    , floodOpacity

    , Tile (..)
    , tileDrawAttributes
    , tileFilterAttr
    , tileIn

    , Offset (..)
    , offsetDrawAttributes
    , offsetFilterAttr
    , offsetIn
    , offsetDX
    , offsetDY

    , MergeNode (..)
    , mergeNodeDrawAttributes
    , mergeNodeIn

    , Merge (..)
    , mergeDrawAttributes
    , mergeFilterAttributes
    , mergeChildren

    , ImageF (..)
    , imageFDrawAttributes
    , imageFFilterAttr
    , imageFHref
    , imageFAspectRatio

    , ComponentTransfer (..)
    , compTransferDrawAttributes
    , compTransferFilterAttr
    , compTransferChildren
    , compTransferIn

    , FuncA (..)
    , FuncType (..)
    , funcADrawAttributes
    , funcAType
    , funcATableValues
    , funcASlope
    , funcAIntercept
    , funcAAmplitude
    , funcAExponent
    , FuncR (..)
    , funcRDrawAttributes
    , funcRType
    , funcRTableValues
    , funcRSlope
    , funcRIntercept
    , funcRAmplitude
    , funcRExponent
    , FuncG (..)
    , funcGDrawAttributes
    , funcGType
    , funcGTableValues
    , funcGSlope
    , funcGIntercept
    , funcGAmplitude
    , funcGExponent
    , FuncB (..)
    , funcBDrawAttributes
    , funcBType
    , funcBTableValues
    , funcBSlope
    , funcBIntercept
    , funcBAmplitude
    , funcBExponent

    , ColorMatrixType(..)
    , colorMatrixDrawAttributes
    , colorMatrixFilterAttr
    , colorMatrixIn
    , colorMatrixType
    , colorMatrixValues
    , ColorMatrix(..)
    , compositeDrawAttributes
    , compositeFilterAttr
    , compositeIn
    , compositeIn2
    , compositeOperator
    , compositeK1
    , compositeK2
    , compositeK3
    , compositeK4
    , Composite(..)
    , CompositeOperator(..)
    , EdgeMode(..)
    , gaussianBlurDrawAttributes
    , gaussianBlurFilterAttr
    , gaussianBlurIn
    , gaussianBlurStdDeviationX
    , gaussianBlurStdDeviationY
    , gaussianBlurEdgeMode
    , GaussianBlur(..)
    , turbulenceDrawAttributes
    , turbulenceFilterAttr
    , turbulenceBaseFrequency
    , turbulenceNumOctaves
    , turbulenceSeed
    , turbulenceStitchTiles
    , turbulenceType
    , Turbulence(..)
    , TurbulenceType(..)
    , StitchTiles(..)
    , DisplacementMap(..)
    , displacementMapDrawAttributes
    , displacementMapFilterAttr
    , displacementMapIn
    , displacementMapIn2
    , displacementMapScale
    , displacementMapXChannelSelector
    , displacementMapYChannelSelector
    , ChannelSelector(..)

      -- * SVG drawing primitives
      -- ** Rectangle
    , Rectangle( .. )
    , pattern RectangleTree
    , rectangleTree
    , rectUpperLeftCorner
    , rectWidth
    , rectHeight
    , rectCornerRadius

      -- ** Line
    , Line( .. )
    , pattern LineTree
    , lineTree
    , linePoint1
    , linePoint2

      -- ** Polygon
    , Polygon( .. )
    , pattern PolygonTree
    , polygonTree
    , polygonPoints

      -- ** Polyline
    , PolyLine( .. )
    , pattern PolyLineTree
    , polyLineTree
    , polyLinePoints

      -- ** Path
    , Path( .. )
    , pattern PathTree
    , pathTree
    , pathDefinition

      -- ** Circle
    , Circle( .. )
    , pattern CircleTree
    , circleTree
    , circleCenter
    , circleRadius


      -- ** Ellipse
    , Ellipse( .. )
    , pattern EllipseTree
    , ellipseTree
    , ellipseCenter
    , ellipseXRadius
    , ellipseYRadius

      -- ** Mesh (gradient mesh)
    , GradientPathCommand( .. )
    , MeshGradientType( .. )

    , MeshGradient( .. )
    , pattern MeshGradientTree
    , meshGradientTree
    , meshGradientX
    , meshGradientY
    , meshGradientType
    , meshGradientUnits
    , meshGradientTransform
    , meshGradientRows

    , MeshGradientRow( .. )
    , meshGradientRowPatches

    , MeshGradientPatch( .. )
    , meshGradientPatchStops

      -- ** Image
    , Image( .. )
    , pattern ImageTree
    , imageTree
    , imageCornerUpperLeft
    , imageWidth
    , imageHeight
    , imageHref
    , imageAspectRatio

      -- ** Use
    , Use( .. )
    , pattern UseTree
    , useTree
    , useBase
    , useName
    , useWidth
    , useHeight

      -- * Grouping primitives
      -- ** Group
    , Group( .. )
    , pattern GroupTree
    , groupTree
    , groupDrawAttributes
    , groupChildren
    , groupViewBox
    , groupAspectRatio

      -- ** Symbol
    , pattern SymbolTree
    , symbolTree

      -- ** Definitions
    , pattern DefinitionTree
    , definitionTree

    -- ** Filter
    , Filter( .. )
    , pattern FilterTree
    , filterTree
    , filterChildren

      -- * Text related types
      -- ** Text
    , Text( .. )
    , pattern TextTree
    , textTree
    , textAdjust
    , textRoot
    , TextAnchor( .. )
    , textAt

      -- ** Text path
    , TextPath( .. )
    , textPathStartOffset
    , textPathName
    , textPathMethod
    , textPathSpacing

    , TextPathSpacing( .. )
    , TextPathMethod( .. )

      -- ** Text span.
    , TextSpanContent( .. )

    , TextSpan( .. )
    , spanInfo
    , spanDrawAttributes
    , spanContent

    , TextInfo( .. )
    , textInfoX
    , textInfoY
    , textInfoDX
    , textInfoDY
    , textInfoRotate
    , textInfoLength

    , TextAdjust( .. )

      -- * Marker definition
    , Marker( .. )
    , pattern MarkerTree
    , markerTree
    , Overflow( .. )
    , MarkerOrientation( .. )
    , MarkerUnit( .. )
    , markerRefPoint
    , markerWidth
    , markerHeight
    , markerOrient
    , markerUnits
    , markerViewBox
    , markerOverflow
    , markerAspectRatio
    , markerElements

      -- * Gradient definition
    , GradientStop( .. )
    , gradientOffset
    , gradientColor
    , gradientPath
    , gradientOpacity

      -- ** Linear Gradient
    , LinearGradient( .. )
    , pattern LinearGradientTree
    , linearGradientTree
    , linearGradientUnits
    , linearGradientStart
    , linearGradientStop
    , linearGradientSpread
    , linearGradientTransform
    , linearGradientStops

      -- ** Radial Gradient
    , RadialGradient( .. )
    , pattern RadialGradientTree
    , radialGradientTree
    , radialGradientUnits
    , radialGradientCenter
    , radialGradientRadius
    , radialGradientFocusX
    , radialGradientFocusY
    , radialGradientSpread
    , radialGradientTransform
    , radialGradientStops

      -- * Pattern definition
    , Pattern( .. )
    , pattern PatternTree
    , patternTree
    , patternViewBox
    , patternWidth
    , patternHeight
    , patternPos
    , patternHref
    , patternElements
    , patternUnit
    , patternAspectRatio
    , patternTransform

      -- * Mask definition
    , Mask( .. )
    , pattern MaskTree
    , maskTree
    , maskContentUnits
    , maskUnits
    , maskPosition
    , maskWidth
    , maskHeight
    , maskContent

      -- * Clip path definition
    , ClipPath( .. )
    , pattern ClipPathTree
    , clipPathTree
    , clipPathUnits
    , clipPathContent

      -- * Aspect Ratio description
    , PreserveAspectRatio( .. )
    , Alignment( .. )
    , MeetSlice( .. )
    , aspectRatioDefer
    , aspectRatioAlign
    , aspectRatioMeetSlice

      -- * MISC functions
    , zipTree
    , foldTree
    , mapTree
    , mapBranch
    , nameOfTree
    , toUserUnit
    , mapNumber
    ) where

import Graphics.SvgTree.Types.Internal
import Graphics.SvgTree.Types.Hashable
import Graphics.SvgTree.Types.Fold
import Graphics.SvgTree.Types.Constructors
import Graphics.SvgTree.Types.Instances ()
