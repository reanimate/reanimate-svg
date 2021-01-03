{-# LANGUAGE CPP #-}
module Graphics.SvgTree.Misc
  ( ppD
  , ppF
  ) where

#if defined(ASTERIUS) || defined(ghcjs_HOST_OS)
import           Numeric
#else
import           Data.Double.Conversion.Text
import qualified Data.Text                   as T
#endif

ppD :: Int -> Double -> String
#if defined(ASTERIUS) || defined(ghcjs_HOST_OS)
ppD precision v = showFFloat (Just precision) v ""
#else
ppD precision = T.unpack . T.dropWhileEnd (== '.') . T.dropWhileEnd (== '0') . toFixed precision
#endif

ppF :: Int -> Float -> String
ppF precision = ppD precision . realToFrac
