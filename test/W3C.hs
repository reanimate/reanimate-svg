{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Graphics.SvgTree hiding (Text)
import Graphics.SvgTree.Printer
import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.IO.Unsafe
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Rerun (defaultMainWithRerun)
import Text.XML.Light
import Text.XML.Light.Input

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      haveFile <- doesFileExist path
      if haveFile
        then do
          raw <- readFile path
          let golden = sanitizeSvg $ normalizeSvg $ sanitizeSvg raw
          let cycled = sanitizeSvg $ normalizeSvg $ sanitizeSvg (cycleSvg golden)
          if golden == cycled
            then putStrLn "Good"
            else do
              putStrLn "Mismatch!"
              putStrLn golden
              putStrLn cycled
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
  return $
    testGroup
      "animate"
      [ testSvg fullPath
        | file <- files,
          let fullPath = path </> file,
          takeExtension fullPath == ".svg"
      ]

testSvg :: FilePath -> TestTree
testSvg path = testCase (takeBaseName path) $ do
  raw <- readFile path
  let golden = sanitizeSvg $ normalizeSvg $ sanitizeSvg raw
  let cycled = sanitizeSvg $ normalizeSvg $ sanitizeSvg (cycleSvg golden)
  assert (golden == cycled)

normalizeSvg :: String -> String
normalizeSvg = unsafePerformIO . readProcess "rsvg-convert" ["--format", "svg"]

-- Sanitize SVG using rsvg-convert
sanitizeSvg :: String -> String
sanitizeSvg inp = --unsafePerformIO (readProcess "rsvg-convert" ["--format", "svg"] inp)
  case parseXMLDoc inp of
    Nothing -> "Failed to parse"
    Just elt ->
      ppElement $
        elt { elContent = mapMaybe filterContent (elContent elt)
            , elAttribs = sort $ mapMaybe filterAttr (elAttribs elt)}
  where
    badAttrs = ["baseProfile", "font-family", "version"]
    badTags = ["SVGTestCase", "title", "font-face", "text"]
    whiteSpaceAttrs = []
    filterContent (Elem elt)
      | qName (elName elt) `elem` badTags = Nothing
      | otherwise = Just $ Elem elt
          { elContent = mapMaybe filterContent (elContent elt)
          , elAttribs = sort $ mapMaybe filterAttr (elAttribs elt)
          , elLine = Nothing}
    filterContent (Text cdata) =
      let txt = dropWhile isSpace $ dropWhileEnd isSpace $ cdData cdata
      in if null txt
        then Nothing
        else Just $ Text
              cdata{ cdData = txt
                  , cdLine = Nothing }
    filterContent cnt = Just cnt
    filterAttr (Attr key val)
      | qPrefix key == Just "xml" = filterAttr $ Attr key{qPrefix = Nothing} val
    filterAttr (Attr key val)
      | qName key `elem` badAttrs = Nothing
      | qName key `elem` whiteSpaceAttrs = Just $ Attr key $ filter (not . isSpace) val
      | val == "#000" = Just $ Attr key "#000000"
    filterAttr attr = Just attr

prettyPrintSvg :: String -> String
prettyPrintSvg = maybe "Failed to parse" (ppElement . filterElement) . parseXMLDoc
  where
    filterElement elt = elt
      { elContent = mapMaybe filterContent (elContent elt)
      , elLine = Nothing}
    filterContent (Elem elt)
      | otherwise = Just $ Elem $ filterElement elt
    filterContent (Text cdata) =
      let txt = dropWhile isSpace $ dropWhileEnd isSpace $ cdData cdata
      in if null txt
        then Nothing
        else Just $ Text
              cdata{ cdData = txt
                  , cdLine = Nothing }
    filterContent cnt = Just cnt

-- Ugly hack to ignore the differences between #FFF and #fff.
lowerHashValues :: String -> String
lowerHashValues ('#' : more) = '#' : map toLower before ++ lowerHashValues after
  where
    (before, after) = splitAt 6 more
lowerHashValues (c : cs) = c : lowerHashValues cs
lowerHashValues [] = []

-- Parse and print SVG with reanimate-svg library.
cycleSvg :: String -> String
cycleSvg =
  maybe "Failed to parse" ppDocument . parseSvgFile "input" . BS.pack
