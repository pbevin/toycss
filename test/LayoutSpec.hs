module LayoutSpec where

import Test.Hspec
import Layout
import Html.HtmlNode
import HtmlDoc
import Dimensions
import Data.Set (Set)
import Css.CssTypes
import Css.ParseCss
import qualified Data.Set as Set
import Prelude hiding (div)

div :: String -> [HtmlNode] -> HtmlNode
div id children = htmlNodeWithId "div" id children

p :: String -> [HtmlNode] -> HtmlNode
p id children = htmlNodeWithId "p" id children

divBig = div "n0" [ Text "hi" ]

para = htmlNodeWithId "p" "n0" [Text "hi"]
para2 = htmlNodeWithId "p" "n1" [Text "hi"]
a1 = htmlNodeWithId "a" "n2" [Text "consectetur adipiscing elit"]
a2 = htmlNodeWithId "a" "n3" [Text "Vestibulum vel ante"]

pInDiv = div "n0" [ p "n1" [ Text "hi" ] ]

spec :: Spec
spec = do
  describe "layout" $ do
    it "lays out an empty document" $ do
      layout (htmlDoc [] []) `shouldBe`
        Set.fromList [ tag "body" $ rect 1024 768 ]

    it "lays out a simple p tag" $ do
      layout (htmlDoc [] [para]) `shouldBe`
        Set.fromList [ tag "body" $ rect 1024 768,
                       tag "n0" $ rect 1024 16 ]

    it "lays out an inline element with text" $ do
      layout (htmlDoc [] [a1]) `shouldBe`
        Set.fromList [ tag "body" $ rect 1024 768,
                       tag "n2" $ rect 166.921875 16 ]
      layout (htmlDoc [] [a2]) `shouldBe`
        Set.fromList [ tag "body" $ rect 1024 768,
                       tag "n3" $ rect 128.765625 16 ]

    it "lays out inline elements on a single line if possible" $
      layout (htmlDoc [] [a1, a2]) `shouldBe`
        Set.fromList [ tag "body" $ rect 1024 768,
                       tag "n2" $ rect 166.921875 16,
                       tag "n3" $ D 0 295.6875 16 166.921875 ]

    it "lays out block elements vertically" $ do
      layout (htmlDoc [] [para, para2]) `shouldBe`
        Set.fromList [ tag "body" $ rect 1024 768,
                       tag "n0" $ rect 1024 16,
                       tag "n1" $ D 16 1024 32 0]

    it "generates empty boxes at the right places" $ do
      layout (htmlDoc [] [para, htmlNodeWithId "p" "n1" []]) `shouldBe`
        Set.fromList [ tag "body" $ rect 1024 768,
                       tag "n0" $ rect 1024 16,
                       tag "n1" $ D 16 1024 16 0 ]

    it "puts contained elements inside their container" $ do
      let doc = [ div "n0" [ Text "a" ],
                  div "n1" [
                    div "n2" [Text "b" ] ] ]

      layout (htmlDoc [] doc) `shouldBe`
        Set.fromList [ tag "body" $ rect 1024 768,
                       tag "n0" $ rect 1024 16,
                       tag "n1" $ D 16 1024 32 0,
                       tag "n2" $ D 16 1024 32 0]

    it "sets a font-size property" $ do
      let css = [ (parseSelector("#n0"), [FontSize (Pct 150)]) ]
      layout (htmlDoc css [divBig]) `shouldBe`
        Set.fromList [ tag "body" $ rect 1024 768,
                       tag "n0" $ rect 1024 24 ]

    it "uses a height property" $ do
      let css = [ (parseSelector("#n0"), [Height (Px 372)]) ]
      layout (htmlDoc css [divBig]) `shouldBe`
        Set.fromList [ tag "body" $ rect 1024 768,
                       tag "n0" $ rect 1024 372 ]

    it "sets a width property" $ do
      let css = [ (parseSelector("#n0"), [Width (Px 320)]) ]
      layout (htmlDoc css [divBig]) `shouldBe`
        Set.fromList [ tag "body" $ rect 1024 768,
                       tag "n0" $ rect 320 16 ]

    it "sets margin-top" $ do
      let css = [ (parseSelector("#n0"), [MarginTop (Px 20)]) ]
      layout (htmlDoc css [divBig]) `shouldBe`
        Set.fromList [ tag "body" $ rect 1024 768,
                       tag "n0" $ D 20 1024 36 0 ]

    it "sets margin-bottom" $ do
      let css = [ (parseSelector("#n0"), [MarginBottom (Px 20)]) ]
      layout (htmlDoc css [divBig, para2]) `shouldBe`
        Set.fromList [ tag "body" $ rect 1024 768,
                       tag "n0" $ D 0 1024 16 0,
                       tag "n1" $ D 36 1024 52 0 ]

    it "sets padding-bottom" $ do
      let css = [ (parseSelector("#n0"), [PaddingBottom (Px 20)]) ]
      layout (htmlDoc css [pInDiv]) `shouldBe`
        Set.fromList [ tag "body" $ rect 1024 768,
                       tag "n0" $ D 0 1024 36 0,
                       tag "n1" $ D 0 1024 16 0 ]


    it "understands display:none" $ do
      let css = [ (parseSelector("#n0"), [Display None]) ]
      layout (htmlDoc css [para]) `shouldBe`
        Set.fromList [ tag "body" $ rect 1024 768,
                       tag "n0" $ rect 0 0 ]
