{-# LANGUAGE OverloadedStrings #-}
import Graphics.SvgTree ( loadSvgFile )
import System.Environment ( getArgs )
import Text.Show.Pretty ( pPrint )

main :: IO ()
main = do
  (f:_) <- getArgs
  loadSvgFile f >>= pPrint
