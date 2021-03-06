module CssSelectorSpec where

import Test.QuickCheck
import Test.Hspec
import Dom

nodeWithClass name cls = Node (NodeAttrs name Nothing [cls])
nodeWithId name id = Node (NodeAttrs name (Just id) [])

spec :: Spec
spec = do
  let p = htmlNode "p" []
  let pp = htmlNode "p" [htmlNode "p" []]
  let bigdiv = nodeWithClass "div" "big" []
  let magicdiv = nodeWithId "div" "magic" []

  describe "locateCss" $ do
    it "can find a tag by its name" $ do
      locateCss "p" p `shouldBe` [p]
      locateCss "p" (htmlNode "div" []) `shouldBe` []
      locateCss "p" (Text "hi") `shouldBe` []

    it "can find a tag inside another tag" $ do
      locateCss "p" (htmlNode "div" [p]) `shouldBe` [p]

    it "can find multiple matching tags" $ do
      locateCss "p" pp `shouldBe` [pp, p]

    it "can find a node with a chained selector" $ do
      locateCss "div p" (htmlNode "div" [p]) `shouldBe` [p]
      locateCss "div p" (htmlNode "p" [p]) `shouldBe` []

    it "can find multiple matching tags" $ do
      locateCss "div p" (htmlNode "div" [p, p]) `shouldBe` [p, p]

    it "can find a node by class" $ do
      locateCss ".big" bigdiv `shouldBe` [bigdiv]

    it "can reject a node by class" $ do
      locateCss ".small" bigdiv `shouldBe` []

    it "can find a node by name and class" $ do
      locateCss "div.big" bigdiv `shouldBe` [bigdiv]
      locateCss "div.small" bigdiv `shouldBe` []
      locateCss "p.big" bigdiv `shouldBe` []

    it "can find a node by ID" $ do
      locateCss "#magic" magicdiv `shouldBe` [magicdiv]
      locateCss "div#magic" magicdiv `shouldBe` [magicdiv]
      locateCss "div#magic" bigdiv `shouldBe` []

    it "recognizes the outer tree" $ property prop_find_outer
    it "works with subtrees" $ property prop_find_multi

prop_find_outer :: HtmlNode -> Bool
prop_find_outer inner =
  let outer = htmlNode "p" [inner]
  in locateCss "p" outer == [outer] ++ locateCss "p" inner

prop_find_multi :: HtmlNode -> Bool
prop_find_multi inner =
  let outer = htmlNode "p" [inner]
  in locateCss "p p" outer == locateCss "p" inner
