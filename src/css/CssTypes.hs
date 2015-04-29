module Css.CssTypes where

type CssSelector = [[CssNodeSpec]]

data CssNodeSpec = Elem String
                 | Class String
                 | Id String
                 deriving (Show, Eq)

data Size = Px Double
          | Pct Double
          | Em Double
          | SizeAuto
          deriving (Show, Eq)

data CssDecl = Display String
             | TextAlign String
             | VerticalAlign String
             | FontSize Size
             | PaddingLeft Size
             | PaddingRight Size
             | PaddingTop Size
             | PaddingBottom Size
             | MarginLeft Size
             | MarginRight Size
             | MarginTop Size
             | MarginBottom Size
             | Width Size
             | Height Size
             deriving (Show, Eq)

type CssRule = (CssSelector, [CssDecl])

data DisplayType = Inline | Block deriving (Show, Eq)

data BoxProperties = BoxProperties { display :: DisplayType,
                                     width :: Size,
                                     height :: Size }
                     deriving (Show, Eq)
