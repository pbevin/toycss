module ShowDom where

import Text.PrettyPrint
import DomNode

ppdom :: DomNode -> String
ppdom = render . showdom

showdom :: DomNode -> Doc
showdom = showdom' 0
  where
    showdom' n (Text t) = text t
    showdom' n node = vcat $
      [ angle (nodeOpen node),
        nest 2 $ cat (map (showdom' $ n+1) $ nodeChildren node),
        angle $ text ("/" ++ nodeName node) ]

nodeOpen :: DomNode -> Doc
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

p :: DomNode -> IO ()
p = putStrLn . ppdom
