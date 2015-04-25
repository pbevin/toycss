module CssSelectorSpec where

import Test.QuickCheck
import Test.Hspec
import Dom
import LocateCss

nodeWithClass name cls = Node (NodeAttrs name Nothing [cls])

spec :: Spec
spec = do
  let p = domNode "p" []
  let pp = domNode "p" [domNode "p" []]
  let bigdiv = nodeWithClass "div" "big" []

  describe "locateCss" $ do
    it "can find a tag by its name" $ do
      locateCss "p" p `shouldBe` [p]
      locateCss "p" (domNode "div" []) `shouldBe` []
      locateCss "p" (Text "hi") `shouldBe` []

    it "can find a tag inside another tag" $ do
      locateCss "p" (domNode "div" [p]) `shouldBe` [p]

    it "can find multiple matching tags" $ do
      locateCss "p" pp `shouldBe` [pp, p]

    it "can find a node with a chained selector" $ do
      locateCss "div p" (domNode "div" [p]) `shouldBe` [p]
      locateCss "div p" (domNode "p" [p]) `shouldBe` []

    it "can find multiple matching tags" $ do
      locateCss "div p" (domNode "div" [p, p]) `shouldBe` [p, p]

    -- it "can find a node by class" $ do
    --   locateCss ".big" bigdiv `shouldBe` [bigdiv]

    -- it "can reject a node by class" $ do
    --   locateCss ".small" bigdiv `shouldBe` []

    -- it "can find a node by name and class" $ do
    --   locateCss "div.big" bigdiv `shouldBe` [bigdiv]
    --   locateCss "div.small" bigdiv `shouldBe` []
    --   locateCss "p.big" bigdiv `shouldBe` []


    it "recognizes the outer tree" $ property prop_find_outer
    it "works with subtrees" $ property prop_find_multi

prop_find_outer :: DomNode -> Bool
prop_find_outer inner =
  let outer = domNode "p" [inner]
  in locateCss "p" outer == [outer] ++ locateCss "p" inner

prop_find_multi :: DomNode -> Bool
prop_find_multi inner =
  let outer = domNode "p" [inner]
  in locateCss "p p" outer == locateCss "p" inner
