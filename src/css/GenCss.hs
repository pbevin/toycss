module Css.GenCss where

import Control.Applicative
import Test.QuickCheck
import Css.CssTypes
import Css.ShowCss

instance Arbitrary CssSelector where
  arbitrary = sized arbSelector

instance Arbitrary CssDecl where
  arbitrary = arbDecl

arbSelector n = oneof [ nodeMatch,
                        CombinedMatch <$> nodeMatch <*> classMatch,
                        CombinedMatch <$> nodeMatch <*> (CombinedMatch <$> classMatch <*> classMatch) ]

nodeMatch  = NameMatch  <$> arbNodeName
classMatch = ClassMatch <$> arbClassName

arbNodeName  = elements [ "a", "div", "span", "p" ]
arbClassName = elements [ "big", "small", "title", "lhs" ]

arbDecl :: Gen CssDecl
arbDecl = oneof [ Display <$> elements [ "block", "inline", "inline-block", "none" ],
                  TextAlign <$> elements [ "left", "right", "center" ],
                  VerticalAlign <$> elements [ "baseline", "sub", "super", "middle", "top", "bottom" ],
                  FontSize <$> arbSize,
                  PaddingLeft <$> arbSize,
                  PaddingRight <$> arbSize,
                  PaddingTop <$> arbSize,
                  PaddingBottom <$> arbSize,
                  Padding <$> arbSize <*> arbSize <*> arbSize <*> arbSize,
                  MarginLeft <$> arbSize,
                  MarginRight <$> arbSize,
                  MarginTop <$> arbSize,
                  MarginBottom <$> arbSize,
                  Margin <$> arbSize <*> arbSize <*> arbSize <*> arbSize,
                  Width <$> arbWH,
                  Height <$> arbWH ]


arbSize = elements [ "1em", "2em", "3em", "4em",
                     "0", "1px", "2px", "5px", "10px",
                     "50%", "150%" ]

arbWH = do
  q <- choose (0, 500) :: Gen Int
  return $ show q ++ "px"



c  = sample' (arbitrary :: Gen CssRule) >>= putStrLn . unlines . map show
cc = sample' (arbitrary :: Gen CssRule) >>= (return . map showcss) >>= putStrLn . unlines
