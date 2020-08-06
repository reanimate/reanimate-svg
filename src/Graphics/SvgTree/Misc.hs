{-# LANGUAGE CPP #-}
module Graphics.SvgTree.Misc
  ( ppD
  , ppF
  ) where

#ifdef ASTERIUS
import           Numeric
#else
import           Data.Double.Conversion.Text
import qualified Data.Text                   as T
#endif

precision :: Int
precision = 6

ppD :: Double -> String
#ifdef ASTERIUS
ppD v = showFFloat (Just precision) v ""
#else
ppD = T.unpack . T.dropWhileEnd (== '.') . T.dropWhileEnd (== '0') . toFixed precision
#endif

ppF :: Float -> String
ppF = ppD . realToFrac
