module PhantomJS where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.IO.Temp
import System.Process
import GHC.IO.Handle
import GenHtml

type Dimensions = [(String, Float, Float, Float, Float)]

layout :: HtmlDoc -> Dimensions
layout = undefined

measure :: HtmlDoc -> IO Dimensions
measure doc = withSystemTempFile "d.html" $ runPhantom doc

runPhantom :: HtmlDoc -> FilePath -> Handle -> IO Dimensions
runPhantom doc filename h = do
  hPutStr h (renderHtml doc)
  hClose h
  let args = ["js/dimensions.js", filename]
  output <- readProcess "phantomjs" args ""


  putStrLn output
  return []


prop_measure :: HtmlDoc -> Property
prop_measure doc = monadicIO $ do
  m <- run $ measure doc
  assert $ m == layout doc


tt = quickCheck prop_measure
