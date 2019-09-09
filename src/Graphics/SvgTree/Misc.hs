module Graphics.SvgTree.Misc where

import Numeric

ppD :: Double -> String
ppD c = pickMin (showFFloat Nothing c "") (showFFloat (Just precision) c "")
  where
    pickMin [] _ = []
    pickMin _ [] = []
    pickMin (x:xs) (_:ys) = x : pickMin xs ys
    precision = 6

ppF :: Float -> String
ppF c = pickMin (showFFloat Nothing c "") (showFFloat (Just precision) c "")
  where
    pickMin [] _ = []
    pickMin _ [] = []
    pickMin (x:xs) (_:ys) = x : pickMin xs ys
    precision = 6
