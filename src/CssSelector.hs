module CssSelector where

data CssSelector = AnyMatch
                 | NameMatch String
                 | ClassMatch String
                 | IdMatch String
                 | CombinedMatch CssSelector CssSelector
                 | DescendantMatch CssSelector CssSelector
                 deriving (Show, Eq)
