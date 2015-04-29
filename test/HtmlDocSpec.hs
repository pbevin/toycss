module HtmlDocSpec where

import Prelude hiding (div)
import Test.Hspec
import HtmlDoc
import Dom
import Debug.Trace

div :: String -> [HtmlNode] -> HtmlNode
div id children = htmlNodeWithId "div" id children

spec :: Spec
spec = do
  it "sets font size from CSS" $ do
    let css = [ (parseSelector "#big", [FontSize (Pct 150)]) ]
    let node = head $ children $ toDom $ htmlDoc css [ div "big" [] ]
    fontSize (properties node) `shouldBe` 24

  it "sets height from CSS" $ do
    let css = [ (parseSelector "#big", [Height (Px 372)]) ]
    let node = head $ children $ toDom $ htmlDoc css [ div "big" [] ]
    height (properties node) `shouldBe` (Px 372)
