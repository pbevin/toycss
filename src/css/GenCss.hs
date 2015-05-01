module Css.GenCss where

import Control.Applicative
import Test.QuickCheck
import Css.CssTypes
import Css.ShowCss

instance Arbitrary CssSelector where
  arbitrary = sized arbSelector

instance Arbitrary CssNodeSpec where
  arbitrary = arbNodeSpec

instance Arbitrary CssDecl where
  arbitrary = arbDecl

arbCssRule n = do
  sel <- arbSelector n
  rules <- resize n (listOf arbDecl)
  return (sel, rules)


arbNodeSpec = frequency [ (1, Elem <$> arbNodeName), (3, Class <$> arbClassName) ]

arbSelector :: Int -> Gen CssSelector
arbSelector n = resize 3 $ Sel <$> listOf1 (listOf1 arbitrary)

arbNodeName  = elements [ "a", "div", "span", "p" ]
arbClassName = elements [ "big", "small", "title", "lhs" ]

arbDecl :: Gen CssDecl
arbDecl = oneof [ Display <$> elements [ Block, Inline, InlineBlock, None ],
                  TextAlign <$> elements [ "left", "right", "center" ],
                  VerticalAlign <$> elements [ "baseline", "sub", "super", "middle", "top", "bottom" ],
                  FontSize <$> arbSize,
                  -- PaddingLeft <$> arbSize,
                  -- PaddingRight <$> arbSize,
                  PaddingTop <$> arbSize,
                  PaddingBottom <$> arbSize,
                  -- MarginLeft <$> arbSize,
                  -- MarginRight <$> arbSize,
                  MarginTop <$> arbSize,
                  MarginBottom <$> arbSize,
                  Width <$> arbWH,
                  Height <$> arbWH ]


arbSize = elements [
  Em 1, Em 2, Em 3, Em 4,
  Px 0, Px 1, Px 2, Px 5, Px 10,
  Pct 50, Pct 100, Pct 150 ]

-- arbSize = elements [ "1em", "2em", "3em", "4em",
--                      "0", "1px", "2px", "5px", "10px",
--                      "50%", "150%" ]

arbWH = do
  size <- choose(0, 500) :: Gen Int
  return $ Px (fromIntegral size)



c  = sample' (arbitrary :: Gen CssRule) >>= putStrLn . unlines . map show
cc = sample' (arbitrary :: Gen CssRule) >>= (return . map showcss) >>= putStrLn . unlines
