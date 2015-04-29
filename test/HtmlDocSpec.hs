module HtmlDocSpec where

import Prelude hiding (div)
import Test.Hspec
import HtmlDoc
import Dom
import Debug.Trace

css = [ (parseSelector "#big", [FontSize (Pct 150)]) ]

div :: String -> [HtmlNode] -> HtmlNode
div id children = htmlNodeWithId "div" id children

spec :: Spec
spec = do
  it "sets font size from CSS" $ do
    let node = head $ children $ toDom $ htmlDoc css [ div "big" [] ]
    fontSize (properties node) `shouldBe` 24
