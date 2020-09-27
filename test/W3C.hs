{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (evaluate, catch, SomeException(..))
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Graphics.SvgTree hiding (Text)
import Graphics.SvgTree.Printer
import Control.Monad
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Unsafe
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Rerun (defaultMainWithRerun)
import Text.XML.Light
import Text.XML.Light.Input
import System.Process.Typed
import Data.String
import Data.ByteString.Lazy (ByteString, fromStrict)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["sort"] -> sortTestSuite
    [path] -> do
      haveFile <- doesFileExist path
      if haveFile
        then do
          pass <- testSvg path
          if pass
            then putStrLn "Good"
            else do
              putStrLn "Mismatch!"
              inp <- readFile path
              let outFile = replaceExtension path "reanimate.svg"
              writeFile outFile (cycleSvg inp)
              putStrLn $ "Reference: " ++ path
              putStrLn $ "Reanimate: " ++ outFile
        else runTestSuite
    _ -> runTestSuite

sortTestSuite :: IO ()
sortTestSuite = do
  goodFiles <- listDirectory "test/good"
  forM_ goodFiles $ \path -> do
    pass <- catchFailure $ testSvg ("test/good" </> path)
    unless pass $ do
      putStrLn $ "Marking as bad: " ++ path
      renameFile ("test/good" </> path) ("test/bad" </> path)
  
  badFiles <- listDirectory "test/bad"
  forM_ badFiles $ \path -> do
    pass <- catchFailure $ testSvg ("test/bad" </> path)
    when pass $ do
      putStrLn $ "Marking as good: " ++ path
      renameFile ("test/bad" </> path) ("test/good" </> path)

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
      [ testCase file $
        testSvg fullPath @? "Rendered file mismatch"
        | file <- files,
          let fullPath = path </> file,
          takeExtension fullPath == ".svg"
      ]

testSvg :: FilePath -> IO Bool
testSvg path = do
  raw <- readFile path
  golden <- generatePng raw
  cycled <- generatePng $ cycleSvg raw
  pure (golden == cycled)

catchFailure :: IO Bool -> IO Bool
catchFailure action = catch action (\SomeException{} -> return False)

generatePng :: String -> IO ByteString
generatePng inp = do
  bs <- evaluate $ fromStrict $ T.encodeUtf8 (T.pack inp)
  (out, _err) <- readProcess_ (setStdin (byteStringInput bs) $ proc "rsvg-convert" ["--format", "png"])
  return out

-- Parse and print SVG with reanimate-svg library.
cycleSvg :: String -> String
cycleSvg =
  maybe "Failed to parse" ppDocument . parseSvgFile "input" . BS.pack
