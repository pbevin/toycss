module PhantomJS where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.IO.Temp
import System.Process
import GHC.IO.Handle
import HtmlDoc
import Dimensions
import GenHtmlDoc
import Layout
import Data.Set (Set)
import qualified Data.Set as Set


measure :: HtmlDoc -> IO (Set TaggedDimensions)
measure doc = withSystemTempFile "d.html" $ runPhantom doc

runPhantom :: HtmlDoc -> FilePath -> Handle -> IO (Set TaggedDimensions)
runPhantom doc filename h = do
  hPutStr h (renderHtml doc)
  hClose h
  let args = ["js/dimensions.js", filename]
  output <- readProcess "phantomjs" args ""

  let dimensions = map read $ filter (not . null) $ lines output

  return $ Set.fromList dimensions

prop_measure :: HtmlDoc -> Property
prop_measure doc = counterexample (renderHtml doc) $ monadicIO $ do
  m <- run $ measure doc
  assert $ m == layout doc

disprove :: HtmlDoc -> IO ()
disprove doc = do
  putStrLn "Document:"
  putStrLn $ renderHtml doc
  putStrLn "PhantomJS dimensions:"
  measure doc >>= putStrLn . unlines . map show . Set.toList
  putStrLn "Layout dimensions:"
  putStr (unlines $ map show $ Set.toList $ layout doc)


tt = quickCheck prop_measure
