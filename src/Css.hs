module Css where

data CssSelector = AnyMatch
                 | NameMatch String
                 | ClassMatch String CssSelector
                 | IdMatch String CssSelector
                 | DescendantMatch CssSelector CssSelector
                 deriving (Show, Eq)
