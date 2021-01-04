{-# LANGUAGE DataKinds #-}
module Main (main) where

import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.List
import Control.Monad
import Control.Exception (evaluate, handle, ErrorCall(..))
import Graphics.SvgTree
import Graphics.SvgTree.Printer
import System.Directory
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.FilePath
import System.Process.Typed
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Rerun (defaultMainWithRerun)

main :: IO ()
-- main = return ()
main = do
  args <- getArgs
  case args of
    ["sort"] -> sortTestCases
    ["cycle", path] -> do
      inp <- readFile path
      putStrLn $ cycleSvg inp
    [path] -> do
      haveFile <- doesFileExist path
      if haveFile
        then do
          passed <- checkCycled path
          if passed
            then putStrLn "Good"
            else putStrLn "Mismatch!"
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
  return $
    testGroup
      "animate"
      [ testSvg fullPath
        | file <- files,
          let fullPath = path </> file,
          takeExtension fullPath == ".svg"
      ]

sortTestCases :: IO ()
sortTestCases = do
  goodFiles <- getDirectoryContents "test/good"
  badFiles <- getDirectoryContents "test/bad"
  forM_ goodFiles $ \file -> when (takeExtension file == ".svg") $ do
    -- putStrLn $ "Processing: " ++ ("test/good" </> file)
    passed <- checkCycled ("test/good" </> file)
    when (not passed) $ do
      putStrLn $ "Marking bad: " ++ file
      renameFile ("test/good" </> file) ("test/bad" </> file)
  forM_ badFiles $ \file -> when (takeExtension file == ".svg") $ do
    -- putStrLn $ "Processing: " ++ ("test/bad" </> file)
    passed <- checkCycled ("test/bad" </> file)
    when passed $ do
      putStrLn $ "Marking good: " ++ file
      renameFile ("test/bad" </> file) ("test/good" </> file)

checkCycled :: FilePath -> IO Bool
checkCycled path =
  handle (\ExitCodeException{} -> return False) $
  handle (\ErrorCall{} -> return False) $ do
    raw <- readFile path
    golden <- sanitizeSvg raw
    cycled <- sanitizeSvg (cycleSvg raw)
    return (golden == cycled)

testSvg :: FilePath -> TestTree
testSvg path = testCase (takeBaseName path) $
  assert =<< checkCycled path

-- Sanitize SVG using rsvg-convert
sanitizeSvg :: String -> IO BS.ByteString
sanitizeSvg inp = do
  let inpT = T.pack inp
  evaluate (T.length inpT)
  (stdout, stderr) <- readProcess_ $
    setStdin (textInput inpT) $
    proc "rsvg-convert" ["--format", "png"]
  return stdout

textInput :: T.Text -> StreamSpec 'STInput ()
textInput = byteStringInput . BS.fromStrict . T.encodeUtf8

-- Ugly hack to ignore the differences between #FFF and #fff.
lowerHashValues :: String -> String
lowerHashValues ('#' : more) = '#' : map toLower before ++ lowerHashValues after
  where
    (before, after) = splitAt 6 more
lowerHashValues (c : cs) = c : lowerHashValues cs
lowerHashValues [] = []

-- prettyPrintSvg :: String -> IO String
-- prettyPrintSvg =
--   fmap lowerHashValues
--     . readProcess
--       "svgo"
--       [ "--input=-",
--         "--output=-",
--         "--pretty",
--         "--enable=sortAttrs,convertColors",
--         "--disable=cleanupIDs,prefixIds,convertTransform,mergePaths,collapseGroups"
--       ]

-- Parse and print SVG with reanimate-svg library.
cycleSvg :: String -> String
cycleSvg =
  maybe "Failed to parse" (ppDocument 6) . parseSvgFile "input" . T.pack
