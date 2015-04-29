module Css.ShowCss where

import Css.CssTypes
import Data.List

showcss :: CssRule -> String
showcss (Sel sel, decls) = intercalate " " (map (concat . map showsel) sel) ++ " { " ++ intercalate " " (map showdecl decls) ++ " }"

showsel :: CssNodeSpec -> String
showsel (Elem n) = n
showsel (Class c) = "." ++ c
showsel (Id id) = "#" ++ id

showdecl :: CssDecl -> String
showdecl d = txt ++ ";"
  where
    txt = case d of
      Display a -> "display: " ++ a
      TextAlign a -> "text-align: " ++ a
      VerticalAlign a -> "vertical-align: " ++ a
      FontSize sz -> "font-size: " ++ showsz sz
      PaddingLeft sz -> "padding-left: " ++ showsz sz
      PaddingRight sz -> "padding-right: " ++ showsz sz
      PaddingTop sz -> "padding-top: " ++ showsz sz
      PaddingBottom sz -> "padding-bottom: " ++ showsz sz
      MarginLeft sz -> "margin-left: " ++ showsz sz
      MarginRight sz -> "margin-right: " ++ showsz sz
      MarginTop sz -> "margin-top: " ++ showsz sz
      MarginBottom sz -> "margin-bottom: " ++ showsz sz
      Width w -> "width: " ++ showsz w
      Height w -> "height: " ++ showsz w

showsz (Px sz) = (show sz) ++ "px"
showsz (Em sz) = (show sz) ++ "em"
showsz (Pct sz) = (show sz) ++ "%"
