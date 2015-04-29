module Html.HtmlNode where

import Text.PrettyPrint

data NodeAttrs =
  NodeAttrs { nName :: String,
              nId :: Maybe String,
              nClass :: [String] }
              deriving (Show, Eq)

data HtmlNode = Text String
              | Node NodeAttrs [HtmlNode]
                deriving (Show,Eq)

attrs name = NodeAttrs name Nothing []

nodeChildren (Node _ children) = children
nodeChildren (Text _) = []

nodeName (Node attrs _) = nName attrs
nodeName (Text _) = "_text"
nodeId (Node attrs _) = nId attrs
nodeId (Text _) = Nothing
nodeClasses (Node attrs _) = nClass attrs
nodeClasses (Text _) = []

setId :: String -> NodeAttrs -> NodeAttrs
setId id attrs = attrs { nId = Just id }

htmlNode :: String -> [HtmlNode] -> HtmlNode
htmlNode name children = Node (attrs name) children

htmlNodeWithId :: String -> String -> [HtmlNode] -> HtmlNode
htmlNodeWithId name id = Node (setId id (attrs name))


alphabet = ['A'..'Z'] ++ ['a'..'z'] ++ " "

alpha = map n alphabet
  where n ch = (htmlNode "p" [htmlNodeWithId "a" [ch] [Text [ch, ' ', ch]]])

junk = map n [ [a,b] | a <- ['a'..'z'], b <- ['a'..'z'] ]
  where n str = (htmlNodeWithId "span" str [Text (str ++ " " ++ str ++ " " ++ str)])
