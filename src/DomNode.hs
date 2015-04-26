module DomNode where

import Text.PrettyPrint

data NodeAttrs =
  NodeAttrs { nName :: String,
              nId :: Maybe String,
              nClass :: [String] }
              deriving (Show, Eq)

data DomNode = Text String
             | Node NodeAttrs [DomNode]
               deriving (Show,Eq)

attrs name = NodeAttrs name Nothing []

nodeChildren (Node _ children) = children
nodeChildren (Text _) = []

nodeName (Node attrs _) = nName attrs
nodeId (Node attrs _) = nId attrs
nodeClasses (Node attrs _) = nClass attrs

setId :: String -> NodeAttrs -> NodeAttrs
setId id attrs = attrs { nId = Just id }

domNode :: String -> [DomNode] -> DomNode
domNode name children = Node (attrs name) children

domNodeWithId :: String -> String -> [DomNode] -> DomNode
domNodeWithId name id = Node (setId id (attrs name))


alphabet = ['A'..'Z'] ++ ['a'..'z'] ++ " "

alpha = map n alphabet
  where n ch = (domNode "p" [domNodeWithId "a" [ch] [Text [ch, ' ', ch]]])

junk = map n [ [a,b] | a <- ['a'..'z'], b <- ['a'..'z'] ]
  where n str = (domNodeWithId "span" str [Text (str ++ " " ++ str ++ " " ++ str)])
