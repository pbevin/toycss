module Dom where

import Control.Applicative
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
  arbitrary = sized arbDomNode

arbDomNode 0 = Text <$> arbText
arbDomNode n = oneof [ Node <$> arbNodeName <*> resize (n-1) (vector 1),
                       Node <$> arbNodeName <*> resize (n `div` 2) (vector 2) ]


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
