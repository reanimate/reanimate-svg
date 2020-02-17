module Graphics.SvgTree.Misc
  ( ppD
  , ppF
  ) where

import           Data.Double.Conversion.Text
import qualified Data.Text                   as T

precision :: Int
precision = 6

ppD :: Double -> String
ppD = T.unpack . T.dropWhileEnd (\c -> c == '.' || c == '0') . toFixed precision

ppF :: Float -> String
ppF = ppD . realToFrac
