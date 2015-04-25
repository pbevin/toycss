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

domNode :: String -> [DomNode] -> DomNode
domNode name children = Node (attrs name) children
