module LayoutSpec where

import Test.Hspec
import Layout
import GenHtmlDoc
import Html.HtmlNode
import Data.Set (Set)
import qualified Data.Set as Set

sq :: Double -> Dimensions
sq sz = rect sz sz

para = htmlNodeWithId "p" "n0" [Text "hi"]
para2 = htmlNodeWithId "p" "n1" [Text "hi"]
a1 = htmlNodeWithId "a" "n0" [Text "consectetur adipiscing elit"]
a2 = htmlNodeWithId "a" "n0" [Text "Vestibulum vel ante"]

spec :: Spec
spec = do
  describe "layout" $ do
    it "lays out an empty document" $ do
      layout (htmldoc [] []) `shouldBe`
        Set.fromList [ tag "body" (rect 1024 768) ]

    it "lays out a simple div" $ do
      layout (htmldoc [] [para]) `shouldBe`
        Set.fromList [ tag "body" (rect 1024 768),
                       tag "n0" (rect 1024 16) ]

    it "lays out an inline element with text" $ do
      layout(htmldoc [] [a1]) `shouldBe`
        Set.fromList [ tag "body" (rect 1024 768),
                       tag "n0" (rect 166.921875 16) ]
      layout(htmldoc [] [a2]) `shouldBe`
        Set.fromList [ tag "body" (rect 1024 768),
                       tag "n0" (rect 128.765625 16) ]

    it "lays out block elements vertically" $ do
      layout (htmldoc [] [para, para2]) `shouldBe`
        Set.fromList [ tag "body" (rect 1024 768),
                       tag "n0" (0, 1024, 16, 0),
                       tag "n1" (16, 1024, 32, 0) ]
