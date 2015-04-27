module Html.GenHtml where

import Test.QuickCheck
import Control.Applicative
import Data.List
import Html.ShowHtml
import Html.HtmlNode

instance Arbitrary HtmlNode where
  arbitrary = sized $ arbNode flowNodes []

arbNode :: [NodeName] -> [NodeName] -> Int -> Gen HtmlNode
arbNode _ _ 0 = Text <$> arbText
arbNode allowed forbidden n =
  oneof [ node 0 n allowed forbidden,
          node 1 n allowed forbidden,
          node 2 n allowed forbidden ]

node :: Int -> Int -> [NodeName] -> [NodeName] -> Gen HtmlNode
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
        children :: NodeName -> Int -> Int -> [NodeName] -> [NodeName] -> Gen [HtmlNode]
        children name size n allowed forbidden = vectorOf size $ do
          let a' = allowed `intersect` (subNodes name)
          let f' = forbidden `union` (forbiddenSubNodes name)
          arbNode a' f' n

arbAttrs :: String -> Gen NodeAttrs
arbAttrs name = NodeAttrs name <$> pure Nothing <*> arbClasses

arbClasses = oneof [ return [], vectorOf 1 arbClassName, vectorOf 2 arbClassName ]
arbClassName = elements [ "big", "small", "title", "lhs" ]

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

s  = sample' (arbitrary :: Gen HtmlNode) >>= putStrLn . unlines . map show
ss = sample' (arbitrary :: Gen HtmlNode) >>= (return . map pphtml) >>= putStrLn . unlines
