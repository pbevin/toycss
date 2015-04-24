module DomSpec where

import Test.QuickCheck
import Test.Hspec
import Dom

spec :: Spec
spec = do
  it "has a size" $ property $
    \dom -> size dom == 1

