module ShowCss where

import CssTypes
import Data.List

showcss :: CssRule -> String
showcss (sel, decls) = showsel sel ++ " { " ++ intercalate "; " (map showdecl decls) ++ " }"

showsel :: CssSelector -> String
showsel (NameMatch n) = n
showsel (ClassMatch c) = "." ++ c
showsel (CombinedMatch a b) = showsel a ++ showsel b

showdecl :: CssDecl -> String
showdecl (Display d) = "display: " ++ d
showdecl (TextAlign a) = "text-align: " ++ a
showdecl (VerticalAlign a) = "vertical-align: " ++ a
showdecl (FontSize sz) = "font-size: " ++ sz
showdecl (PaddingLeft sz) = "padding-left: " ++ sz
showdecl (PaddingRight sz) = "padding-right: " ++ sz
showdecl (PaddingTop sz) = "padding-top: " ++ sz
showdecl (PaddingBottom sz) = "padding-bottom: " ++ sz
showdecl (Padding a b c d) = "padding: " ++ intercalate " " [a,b,c,d]
showdecl (MarginLeft sz) = "margin-left: " ++ sz
showdecl (MarginRight sz) = "margin-right: " ++ sz
showdecl (MarginTop sz) = "margin-top: " ++ sz
showdecl (MarginBottom sz) = "margin-bottom: " ++ sz
showdecl (Margin a b c d) = "margin: " ++ intercalate " " [a,b,c,d]
showdecl (Width w) = "width: " ++ w
showdecl (Height w) = "height: " ++ w
