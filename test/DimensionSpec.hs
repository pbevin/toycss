module DimensionSpec where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Hspec
import PhantomJS
import GenHtml


spec :: Spec
spec = do
  it "runs phantomjs" $ property prop_measure

