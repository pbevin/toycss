module ShowDom where

import Text.PrettyPrint
import DomNode

ppdom :: DomNode -> String
ppdom = render . ppdom' 0
  where
    ppdom' n (Text t) = text t
    ppdom' n node = vcat $
      [ angle (nodeOpen node),
        nest 2 $ cat (map (ppdom' $ n+1) $ nodeChildren node),
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
