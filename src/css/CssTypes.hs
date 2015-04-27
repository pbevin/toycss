module Css.CssTypes where

data CssSelector = AnyMatch
                 | NameMatch String
                 | ClassMatch String
                 | IdMatch String
                 | CombinedMatch CssSelector CssSelector
                 | DescendantMatch CssSelector CssSelector
                 deriving (Show, Eq)

data CssDecl = Display String
             | TextAlign String
             | VerticalAlign String
             | FontSize String
             | PaddingLeft String
             | PaddingRight String
             | PaddingTop String
             | PaddingBottom String
             | Padding String String String String
             | MarginLeft String
             | MarginRight String
             | MarginTop String
             | MarginBottom String
             | Margin String String String String
             | Width String
             | Height String
             deriving (Show, Eq)

type CssRule = (CssSelector, [CssDecl])
