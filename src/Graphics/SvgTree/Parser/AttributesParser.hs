{-# LANGUAGE OverloadedStrings #-}

module Graphics.SvgTree.Parser.AttributesParser where

import Graphics.SvgTree.Types
import Graphics.SvgTree.Types.Attributes
import Graphics.SvgTree.Types.Contents
import Graphics.SvgTree.Parser.ContentsParser

import           Data.Scientific            (toRealFloat)
import qualified Data.Text                  as T
import           Control.Applicative        ((<|>))

import           Data.Attoparsec.Text (Parser, char, digit, many1,
                                       parseOnly, scientific, skipSpace,
                                       string)


-- accent-height
-- accumulate
-- additive
-- alignment-baseline
-- allowReorder
-- alphabetic
-- amplitude
-- arabic-form
-- ascent
-- attributeName
-- attributeType
-- autoReverse
-- azimuth

-- baseFrequency
-- baseline-shift
-- baseProfile
-- bbox
-- begin
-- bias
-- by

-- calcMode
-- cap-height
-- class
-- clip
-- clipPathUnits
-- clip-path
-- clip-rule
-- color
-- color-interpolation
-- color-interpolation-filters
-- color-profile
-- color-rendering
-- contentScriptType
-- contentStyleType
-- cursor
-- cx
-- cy

-- d
-- decelerate
-- descent
-- diffuseConstant
-- direction
-- display
-- divisor
-- dominant-baseline
-- dur
-- dx
-- dy

-- edgeMode
-- elevation
-- enable-background
-- end
-- exponent
-- externalResourcesRequired

-- fill
-- fill-opacity
-- fill-rule
-- filter
-- filterRes
-- filterUnits
-- flood-color
-- flood-opacity
-- font-family
-- font-size
-- font-size-adjust
-- font-stretch
-- font-style
-- font-variant
-- font-weight
-- format
-- from
-- fr
-- fx
-- fy

-- g1
-- g2
-- glyph-name
-- glyph-orientation-horizontal
-- glyph-orientation-vertical
-- glyphRef
-- gradientTransform
-- gradientUnits

-- hanging
-- height
-- href
-- hreflang
-- horiz-adv-x
-- horiz-origin-x

-- id
-- ideographic
-- image-rendering
-- in
-- in2
-- intercept
-- K
-- k
-- k1
-- k2
-- k3
-- k4
-- kernelMatrix
-- kernelUnitLength
-- kerning
-- keyPoints
-- keySplines
-- keyTimes

-- lang
-- lengthAdjust
-- letter-spacing
-- lighting-color
-- limitingConeAngle
-- local

-- marker-end
-- marker-mid
-- marker-start
-- markerHeight
-- markerUnits
-- markerWidth
-- mask
-- maskContentUnits
-- maskUnits
-- mathematical
-- max
-- media
-- method
-- min
-- mode
-- N
-- name
-- numOctaves

-- offset
-- opacity
-- operator
-- order
-- orient
-- orientation
-- origin
-- overflow
-- overline-position
-- overline-thickness

-- panose-1
-- paint-order
-- path
-- pathLength
-- patternContentUnits
-- patternTransform
-- patternUnits
-- ping
-- pointer-events
-- points
-- pointsAtX
-- pointsAtY
-- pointsAtZ
-- preserveAlpha
-- preserveAspectRatio
-- primitiveUnits

-- r
-- radius
-- referrerPolicy
-- refX
-- refY
-- rel
-- rendering-intent
-- repeatCount
-- repeatDur
-- requiredExtensions
-- requiredFeatures
-- restart
-- result
-- rotate
-- rx
-- ry

-- scale
-- seed
-- shape-rendering
-- slope
-- spacing
-- specularConstant
-- specularExponent
-- speed
-- spreadMethod
-- startOffset
-- stdDeviation
-- stemh
-- stemv
-- stitchTiles
-- stop-color
-- stop-opacity
-- strikethrough-position
-- strikethrough-thickness
-- string
-- stroke
-- stroke-dasharray
-- stroke-dashoffset
-- stroke-linecap
-- stroke-linejoin
-- stroke-miterlimit
-- stroke-opacity
-- stroke-width
-- style
-- surfaceScale
-- systemLanguage

-- tabindex
-- tableValues
-- target
-- targetX
-- targetY
-- text-anchor
-- text-decoration
-- text-rendering
-- textLength
-- to
-- transform
-- transform-origin
-- type

-- u1
-- u2
-- underline-position
-- underline-thickness
-- unicode
-- unicode-bidi
-- unicode-range
-- units-per-em

-- v-alphabetic
-- v-hanging
-- v-ideographic
-- v-mathematical
-- values
-- vector-effect
-- version
-- vert-adv-y
-- vert-origin-x
-- vert-origin-y
-- viewBox
viewBoxParser :: Parser ViewBox
viewBoxParser =  viewBox
       <$> iParse <*> iParse <*> iParse <*> iParse
  where
    iParse = numberParser <* skipSpace
    viewBox a b c d = ViewBox $ Just (a,b,c,d)

-- viewTarget
-- visibility

-- width
-- widths
-- word-spacing
-- writing-mode

-- x
-- x-height
-- x1
-- x2
-- xChannelSelector
-- xlink:actuate
-- xlink:arcrole
-- xlink:href
-- xlink:role
-- xlink:show
-- xlink:title
-- xlink:type
-- xml:base
-- xml:lang
-- xml:space

-- y
-- y1
-- y2
-- yChannelSelector

-- z
-- zoomAndPan
