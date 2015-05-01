module Css.CssTypes where

import Dimensions

data CssSelector = Sel [[CssNodeSpec]] deriving (Show, Eq)

unsel :: CssSelector -> [[CssNodeSpec]]
unsel (Sel nss) = nss

data CssNodeSpec = Elem String
                 | Class String
                 | Id String
                 deriving (Show, Eq)

data Size = Px Double
          | Pct Double
          | Em Double
          | SizeAuto
          deriving (Show, Eq)

data CssDecl = Display DisplayType
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

data DisplayType = Inline | Block | InlineBlock | None deriving (Show, Eq)

data BoxProperties = BoxProperties { display :: DisplayType,
                                     width :: Size,
                                     height :: Size,
                                     marginTop :: Size,
                                     marginBottom :: Size,
                                     paddingTop :: Size,
                                     paddingBottom :: Size,
                                     fontSize :: Height }
                     deriving (Show, Eq)

initialProperties :: BoxProperties
initialProperties = BoxProperties { display = Block,
                                    width = SizeAuto,
                                    height = SizeAuto,
                                    marginTop = Px 0,
                                    marginBottom = Px 0,
                                    paddingTop = Px 0,
                                    paddingBottom = Px 0,
                                    fontSize = 16 }
