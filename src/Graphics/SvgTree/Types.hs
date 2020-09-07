{-# LANGUAGE PatternSynonyms    #-}
-- | This module define all the types used in the definition
-- of a svg scene.
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
    , svgTree
    , viewBox
    , width
    , height
    , elements
    , description
    , documentLocation
    , documentAspectRatio
    , documentSize

      -- * Drawing attributes
    , DrawAttributes( .. )
    , HasDrawAttributes( .. )

      -- * Filters
    , FilterElement(..)
    , FilterAttributes(..)
    , HasFilterAttributes(..)
    , FilterSource(..)
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
    , rectangleTree
    , rectUpperLeftCorner
    , rectWidth
    , rectHeight
    , rectCornerRadius

      -- ** Line
    , Line( .. )
    , lineTree
    , linePoint1
    , linePoint2

      -- ** Polygon
    , Polygon( .. )
    , polygonTree
    , polygonPoints

      -- ** Polyline
    , PolyLine( .. )
    , polyLineTree
    , polyLinePoints

      -- ** Path
    , Path( .. )
    , pathTree
    , pathDefinition

      -- ** Circle
    , Circle( .. )
    , circleTree
    , circleCenter
    , circleRadius


      -- ** Ellipse
    , Ellipse( .. )
    , ellipseTree
    , ellipseCenter
    , ellipseXRadius
    , ellipseYRadius

      -- ** Mesh (gradient mesh)
    , GradientPathCommand( .. )
    , MeshGradientType( .. )

    , MeshGradient( .. )
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
    , imageTree
    , imageCornerUpperLeft
    , imageWidth
    , imageHeight
    , imageHref
    , imageAspectRatio

      -- ** Use
    , Use( .. )
    , useBase
    , useName
    , useWidth
    , useHeight

      -- * Grouping primitives
      -- ** Group
    , Group( .. )
    , groupTree
    , HasGroup( .. )

      -- ** Symbol
    , Symbol( .. )
    , symbolTree
    , groupOfSymbol

      -- ** Definitions
    , Definitions( .. )
    , definitionTree
    , groupOfDefinitions

    -- ** Filter
    , Filter( .. )
    , filterTree
    , filterChildren

      -- * Text related types
      -- ** Text
    , Text( .. )
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
    , linearGradientTree
    , linearGradientUnits
    , linearGradientStart
    , linearGradientStop
    , linearGradientSpread
    , linearGradientTransform
    , linearGradientStops

      -- ** Radial Gradient
    , RadialGradient( .. )
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
    , maskTree
    , maskContentUnits
    , maskUnits
    , maskPosition
    , maskWidth
    , maskHeight
    , maskContent

      -- * Clip path definition
    , ClipPath( .. )
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
    , nameOfTree
    , toUserUnit
    , mapNumber
    ) where

import Graphics.SvgTree.Types.Internal
import Graphics.SvgTree.Types.Hashable
import Graphics.SvgTree.Types.Fold
import Graphics.SvgTree.Types.Constructors
import Graphics.SvgTree.Types.Instances ()