module Main (main) where

import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun (defaultMainWithRerun)
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.HUnit
import qualified Data.ByteString.Char8 as BS
import Graphics.SvgTree
import Graphics.SvgTree.Printer
import System.Process
import System.Directory
import System.FilePath
import Data.List
import System.Environment
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Data.Char

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      haveFile <- doesFileExist path
      if haveFile
        then do
          raw <- readFile path
          golden <- prettyPrintSvg =<< sanitizeSvg raw
          cycled <- prettyPrintSvg (cycleSvg golden)
          if golden == cycled
            then putStrLn "Good"
            else do
              putStrLn "Mismatch!"
              let d = getGroupedDiff (lines golden) (lines cycled) :: [Diff [String]]
                  ranges = diffToLineRanges d
              print (prettyDiffs ranges)
        else runTestSuite
    _ -> runTestSuite

runTestSuite :: IO ()
runTestSuite = do
  print =<< getCurrentDirectory
  good <- unitTestFolder "test/good"
  bad <- unitTestFolder "test/bad"
  defaultMainWithRerun $ testGroup "tests" [good, expectFail bad]

unitTestFolder :: FilePath -> IO TestTree
unitTestFolder path = do
  files <- sort <$> getDirectoryContents path
  return $ testGroup "animate"
    [ testSvg fullPath
    | file <- files
    , let fullPath = path </> file
    , takeExtension fullPath == ".svg"
    ]

testSvg :: FilePath -> TestTree
testSvg path = testCase (takeBaseName path) $ do
  raw <- readFile path
  golden <- prettyPrintSvg =<< sanitizeSvg raw
  cycled <- prettyPrintSvg (cycleSvg golden)
  assert (golden == cycled)

-- Sanitize SVG using rsvg-convert
sanitizeSvg :: String -> IO String
sanitizeSvg = readProcess "rsvg-convert" ["--format", "svg"]

-- Ugly hack to ignore the differences between #FFF and #fff.
lowerHashValues :: String -> String
lowerHashValues ('#':more) = '#' : map toLower before ++ lowerHashValues after
  where
    (before, after) = splitAt 6 more
lowerHashValues (c:cs) = c:lowerHashValues cs
lowerHashValues [] = []

prettyPrintSvg :: String -> IO String
prettyPrintSvg = fmap lowerHashValues .
  readProcess "svgo" ["--input=-", "--output=-", "--pretty"
    , "--enable=sortAttrs,convertColors"
    , "--disable=cleanupIDs,prefixIds,convertTransform,mergePaths,collapseGroups" ]

-- Parse and print SVG with reanimate-svg library.
cycleSvg :: String -> String
cycleSvg = 
  maybe "Failed to parse" ppDocument . parseSvgFile "input" . BS.pack
