module Dom where

import Control.Applicative
import Data.List
import Test.QuickCheck
import Text.PrettyPrint

data DomNode = Node String [DomNode] | Text String deriving (Show,Eq)

ppdom :: DomNode -> String
ppdom = render . ppdom' 0
  where
    ppdom' n (Text t) = text t
    ppdom' n (Node name children) = cat $
      [ angle name,
        nest (2*n+2) $ vcat (map (ppdom' $ n+1) children),
        angle ("/" ++ name) ]

angle :: String -> Doc
angle name = text ("<" ++ name ++ ">")

instance Arbitrary DomNode where
  arbitrary = sized $ arbNode flowNodes []


arbNode :: [NodeName] -> [NodeName] -> Int -> Gen DomNode
arbNode _ _ 0 = Text <$> arbText
arbNode allowed forbidden n =
  oneof [ node 0 n allowed forbidden,
          node 1 n allowed forbidden,
          node 2 n allowed forbidden ]

node :: Int -> Int -> [NodeName] -> [NodeName] -> Gen DomNode
node size n allowed forbidden = do
  name <- elements allowed
  if size == 0
  then return $ Node name
  else do
    let n' = (n-1) `div` size
    subs <- children name size n' allowed forbidden
    return $ Node name subs

children :: NodeName -> Int -> Int -> [NodeName] -> [NodeName] -> Gen [DomNode]
children name size n allowed forbidden = vectorOf size $ do
  let a' = allowed `intersect` (subNodes name)
  let f' = forbidden `union` (forbiddenSubNodes name)
  arbNode a' f' n

type NodeName = String
subNodes :: NodeName -> [NodeName]
subNodes "body" = flowNodes
subNodes "a" = flowNodes `union` phrasingNodes
subNodes "form" = flowNodes
subNodes _ = phrasingNodes

forbiddenSubNodes :: NodeName -> [NodeName]
forbiddenSubNodes "form" = ["form"]
forbiddenSubNodes _ = []


flowNodes = [ "a", "p", "h1", "h2", "h3", "form", "div", "span" ]
flowNodesNoForm = [ "a", "p", "h1", "h2", "h3", "form", "div", "span" ]
phrasingNodes = [ "span" ]





-- flowNodes = [ "a", "abbr", "address", "article", "aside", "audio", "b","bdo", "bdi", "blockquote", "br", "button", "canvas", "cite", "code", "command", "data", "datalist", "del", "details", "dfn", "div", "dl", "em", "embed", "fieldset", "figure", "footer", "form", "h1", "h2", "h3", "h4", "h5", "h6", "header", "hgroup", "hr", "i", "iframe", "img", "input", "ins", "kbd", "keygen", "label", "main", "map", "mark", "math", "menu", "meter", "nav", "noscript", "object", "ol", "output", "p", "pre", "progress", "q", "ruby", "s", "samp", "script", "section", "select", "small", "span", "strong", "sub", "sup", "svg", "table", "template", "textarea", "time", "ul", "var", "video", "wbr" ]
-- sectionNodes = [ "article", "aside", "nav", "section" ]
-- headingNodes = [ "h1", "h2", "h3", "h4", "h5", "h6", "hgroup" ]
-- phrasingNodes = [ "abbr", "audio", "b", "bdo", "br", "button", "canvas", "cite", "code", "command", "datalist", "dfn", "em", "embed", "i", "iframe", "img", "input", "kbd", "keygen", "label", "mark", "math", "meter", "noscript", "object", "output", "progress", "q", "ruby", "samp", "script", "select", "small", "span", "strong", "sub", "sup", "svg", "textarea", "time", "var", "video", "wbr" ]
-- formNodes = [ "button", "fieldset", "input", "keygen", "label", "meter", "object", "output", "progress", "select", "textarea" ]

arbNodeName :: Gen String
arbNodeName = elements [ "h1", "h2", "div", "span", "form", "p", "a" ]

arbNodeNameNoContent :: Gen String
arbNodeNameNoContent = elements [ "input" ]

arbText = elements ["Lorem Ipsum", "Dolor sit amet", "..."]


size :: DomNode -> Int
size n = 1

prop_hasSize :: DomNode -> Bool
prop_hasSize node = size node == 2

s = sample (arbitrary :: Gen DomNode)

p :: DomNode -> IO ()
p = putStrLn . ppdom
