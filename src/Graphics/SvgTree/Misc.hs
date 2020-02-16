module Graphics.SvgTree.Misc
  ( ppD
  , ppF
  ) where

import           Data.Double.Conversion.Text
import qualified Data.Text                   as T

precision :: Int
precision = 6

ppD :: Double -> String
ppD c = T.unpack $ T.take (precision+2) $ toShortest c

ppF :: Float -> String
ppF = ppD . realToFrac
