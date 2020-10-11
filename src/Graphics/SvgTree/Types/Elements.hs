{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.SvgTree.Types.Elements where

--import Graphics.SvgTree.Types.Basic
--import Graphics.SvgTree.CssTypes
import Graphics.SvgTree.Types.Attributes
--import Graphics.SvgTree.Types.Contents

import Control.Lens.TH (makeLenses)
import GHC.Generics (Generic)
import Data.Hashable


-- | Main type to describe the SVG structure --
data Tree = CachedTree
  { _treeBranch :: TreeBranch,
    _treeHash :: Int
  }
  deriving (Eq, Show, Generic, Hashable)

data TreeBranch
  = NoNode
  | CircleNode Circle
  deriving (Eq, Show, Generic, Hashable)


-- <a>
-- <animate>
-- <animateMotion>
-- <animateTransform>

-- <circle>
data Circle = Circle
  { _circleCoreAttributes :: CoreAttributes,
    _circleStylingAttributes :: StylingAttributes,
    _circleConditionalProcessingAttributes :: ConditionalProcessingAttributes,
    _circlePresentationAttributes :: PresentationAttributes,
    _circleCx :: Maybe Cx,
    _circleCy :: Maybe Cy,
    _circleR :: Maybe RAttr,
    _circlePathLength :: Maybe PathLength,
    _circleContent :: Maybe [Tree]
  }
  deriving (Eq, Show, Generic, Hashable)

instance WithDefaultSvg Circle where
  defaultSvg =
    Circle
    { _circleCoreAttributes = defaultSvg,
      _circleStylingAttributes = defaultSvg,
      _circleConditionalProcessingAttributes = defaultSvg,
      _circlePresentationAttributes = defaultSvg,
      _circleCx = Nothing,
      _circleCy = Nothing,
      _circleR = Nothing,
      _circlePathLength = Nothing,
      _circleContent = Nothing
    }

-- <clipPath>
-- <color-profile>

-- <defs>
-- <desc>
-- <discard>

-- <ellipse>
data Ellipse = Ellipse
  { _ellipseCoreAttributes :: CoreAttributes,
    _ellipseStylingAttributes :: StylingAttributes,
    _ellipseConditionalProcessingAttributes :: ConditionalProcessingAttributes,
    _ellipsePresentationAttributes :: PresentationAttributes,
    _ellipseCx :: Maybe Cx,
    _ellipseCy :: Maybe Cy,
    _ellipseRx :: Maybe Rx,
    _ellipseRy :: Maybe Ry,
    _ellipsePathLength :: Maybe PathLength,
    _ellipseContent :: Maybe [Tree]
  }
  deriving (Eq, Show, Generic, Hashable)

instance WithDefaultSvg Ellipse where
  defaultSvg =
    Ellipse
    { _ellipseCoreAttributes = defaultSvg,
      _ellipseStylingAttributes = defaultSvg,
      _ellipseConditionalProcessingAttributes = defaultSvg,
      _ellipsePresentationAttributes = defaultSvg,
      _ellipseCx = Nothing,
      _ellipseCy = Nothing,
      _ellipseRx = Nothing,
      _ellipseRy = Nothing,
      _ellipsePathLength = Nothing,
      _ellipseContent = Nothing
    }


-- <feBlend>
-- <feColorMatrix>
-- <feComponentTransfer>
-- <feComposite>
-- <feConvolveMatrix>
-- <feDiffuseLighting>
-- <feDisplacementMap>
-- <feDistantLight>
-- <feDropShadow>
-- <feFlood>
-- <feFuncA>
-- <feFuncB>
-- <feFuncG>
-- <feFuncR>
-- <feGaussianBlur>
-- <feImage>
-- <feMerge>
-- <feMergeNode>
-- <feMorphology>
-- <feOffset>
-- <fePointLight>
-- <feSpecularLighting>
-- <feSpotLight>
-- <feTile>
-- <feTurbulence>
-- <filter>
-- <foreignObject>

-- <g>

-- <hatch>
-- <hatchpath>

-- <image>

-- <line>
-- <linearGradient>

-- <marker>
-- <mask>
-- <mesh>
-- <meshgradient>
-- <meshpatch>
-- <meshrow>
-- <metadata>
-- <mpath>

-- <path>
-- <pattern>
-- <polygon>
-- <polyline>

-- <radialGradient>
-- <rect>
data Rect = Rect
  { _rectCoreAttributes :: CoreAttributes,
    _rectStylingAttributes :: StylingAttributes,
    _rectConditionalProcessingAttributes :: ConditionalProcessingAttributes,
    _rectPresentationAttributes :: PresentationAttributes,
    _rectX :: (Maybe XAttr),
    _rectY :: (Maybe YAttr),
    _rectWidth :: (Maybe WidthAttr),
    _rectRx :: (Maybe Rx),
    _rectRy :: (Maybe Ry),
    _rectPathLength :: (Maybe PathLength)
  }
  deriving (Eq, Show, Generic, Hashable)

instance WithDefaultSvg Rect where
  defaultSvg =
    Rect
    { _rectCoreAttributes = defaultSvg,
    _rectStylingAttributes = defaultSvg,
    _rectConditionalProcessingAttributes = defaultSvg,
    _rectPresentationAttributes = defaultSvg,
    _rectX = Nothing,
    _rectY = Nothing,
    _rectWidth = Nothing,
    _rectRx = Nothing,
    _rectRy = Nothing,
    _rectPathLength = Nothing
    }

-- <script>
-- <set>
-- <solidcolor>
-- <stop>
-- <style>
-- <svg>
data SVG = SVG
  { _svgCoreAttributes :: CoreAttributes,
    _svgStylingAttributes :: StylingAttributes,
    _svgConditionalProcessingAttributes :: ConditionalProcessingAttributes,
    _svgPresentationAttributes :: PresentationAttributes,
    _svgBaseProfile :: (Maybe BaseProfile),
--    _svgContentScriptType :: (Maybe ContentScriptType).
--    _svgContentStyleType :: (Maybe ContentStayleType),
    _svgHeight :: (Maybe HeightAttr),
    _svgWidth :: (Maybe WidthAttr),
    _svgPreserveAspectRatio :: (Maybe PreserveAspectRatio),
    _svgVersion :: (Maybe Version),
    _svgViewBox :: (Maybe ViewBox),
    _svgX :: (Maybe XAttr),
    _svgY :: (Maybe YAttr)
  }
  deriving (Eq, Show, Generic, Hashable)

instance WithDefaultSvg SVG where
  defaultSvg =
    SVG
    { _svgCoreAttributes = defaultSvg,
      _svgStylingAttributes = defaultSvg,
      _svgConditionalProcessingAttributes = defaultSvg,
      _svgPresentationAttributes = defaultSvg,
      _svgBaseProfile = Nothing,
      --    _svgContentScriptType = Nothing,
      --    _svgContentStyleType = Nothing,
      _svgHeight = Nothing,
      _svgWidth = Nothing,
      _svgPreserveAspectRatio = Nothing,
      _svgVersion = Nothing,
      _svgViewBox = Nothing,
      _svgX = Nothing,
      _svgY = Nothing
    }

-- <switch>
-- <symbol>

-- <text>
-- <textPath>
-- <title>
-- <tspan>

-- <unknown>
-- <use>

-- <view>


-- Lenses declarations
makeLenses ''Tree
makeLenses ''Circle
makeLenses ''Ellipse
makeLenses ''Rect
makeLenses ''SVG
