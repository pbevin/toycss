module Html.ShowHtml where

import Text.PrettyPrint
import Html.HtmlNode

pphtml :: HtmlNode -> String
pphtml = render . showhtml

showhtml :: HtmlNode -> Doc
showhtml = showhtml' 0
  where
    showhtml' n (Text t) = text t
    showhtml' n node = vcat $
      [ angle (nodeOpen node),
        nest 2 $ cat (map (showhtml' $ n+1) $ nodeChildren node),
        angle $ text ("/" ++ nodeName node) ]

nodeOpen :: HtmlNode -> Doc
nodeOpen (Node attrs _) =
  sep  [ text $ nName attrs,
         maybe empty (attr "id") (nId attrs),
         attr "class" (unwords $ nClass attrs) ]

attr :: String -> String -> Doc
attr key value = if null value
                 then empty
                 else text key <> text "=" <> doubleQuotes (text value)


angle :: Doc -> Doc
angle name = text "<" <> name <> text ">"

p :: HtmlNode -> IO ()
p = putStrLn . pphtml
