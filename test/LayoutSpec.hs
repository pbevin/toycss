module LayoutSpec where

import Test.Hspec
import Layout
import Html.HtmlNode
import HtmlDoc
import Dimensions
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (div)

sq :: Double -> Dimensions
sq sz = rect sz sz

div :: String -> [HtmlNode] -> HtmlNode
div id children = htmlNodeWithId "div" id children

para = htmlNodeWithId "p" "n0" [Text "hi"]
para2 = htmlNodeWithId "p" "n1" [Text "hi"]
a1 = htmlNodeWithId "a" "n0" [Text "consectetur adipiscing elit"]
a2 = htmlNodeWithId "a" "n0" [Text "Vestibulum vel ante"]

spec :: Spec
spec = do
  describe "layout" $ do
    it "lays out an empty document" $ do
      layout (htmlDoc [] []) `shouldBe`
        Set.fromList [ tag "body" (rect 1024 768) ]

    it "lays out a simple div" $ do
      layout (htmlDoc [] [para]) `shouldBe`
        Set.fromList [ tag "body" (rect 1024 768),
                       tag "n0" (rect 1024 16) ]

    it "lays out an inline element with text" $ do
      layout (htmlDoc [] [a1]) `shouldBe`
        Set.fromList [ tag "body" (rect 1024 768),
                       tag "n0" (rect 166.921875 16) ]
      layout (htmlDoc [] [a2]) `shouldBe`
        Set.fromList [ tag "body" (rect 1024 768),
                       tag "n0" (rect 128.765625 16) ]

    it "lays out block elements vertically" $ do
      layout (htmlDoc [] [para, para2]) `shouldBe`
        Set.fromList [ tag "body" (rect 1024 768),
                       tag "n0" (0, 1024, 16, 0),
                       tag "n1" (16, 1024, 32, 0) ]
