module Dom where

import Control.Applicative hiding (empty)
import Data.List
import Test.QuickCheck
import Text.PrettyPrint
import Debug.Trace

data NodeAttrs = NodeAttrs { nName :: String,
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
  name <- elements (allowed \\ forbidden)
  if size == 0
  then frequency [ (1, Node <$> arbAttrs name <*> pure []),
                   (3, nodeWithText name) ]
  else do
    let n' = (n-1) `div` size
    subs <- children name size n' allowed forbidden
    attrs <- arbAttrs name
    return $ Node attrs subs
      where
        children :: NodeName -> Int -> Int -> [NodeName] -> [NodeName] -> Gen [DomNode]
        children name size n allowed forbidden = vectorOf size $ do
          let a' = allowed `intersect` (subNodes name)
          let f' = forbidden `union` (forbiddenSubNodes name)
          arbNode a' f' n

arbAttrs :: String -> Gen NodeAttrs
arbAttrs name = NodeAttrs name <$> arbId <*> arbClasses

arbId = frequency [ (3, pure Nothing), (1, Just <$> arbIdent) ]
arbIdent = do
  x <- choose ('a', 'z')
  return [x]

arbClasses = oneof [ return [], vectorOf 1 arbIdent, vectorOf 2 arbIdent ]

nodeWithText name = do
  attrs <- arbAttrs name
  text <- Text <$> arbText
  return $ Node attrs [text]

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

arbText = elements [ "Lorem Ipsum",
                     "Dolor sit amet",
                     "consectetur adipiscing elit",
                     "Vestibulum vel ante",
                     "ut turpis dapibus blandit" ]

size :: DomNode -> Int
size n = 1

prop_hasSize :: DomNode -> Bool
prop_hasSize node = size node == 2

s  = sample' (arbitrary :: Gen DomNode) >>= putStrLn . unlines . map show
ss = sample' (arbitrary :: Gen DomNode) >>= (return . map ppdom) >>= putStrLn . unlines

p :: DomNode -> IO ()
p = putStrLn . ppdom
