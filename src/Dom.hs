module Dom where

import Control.Applicative
import Test.QuickCheck

data DomNode = Node String [DomNode] | Text String deriving (Show,Eq)

instance Arbitrary DomNode where
  arbitrary = sized arbDomNode

arbDomNode 0 = oneof [ Node <$> arbNodeName <*> pure [], Text <$> arbitrary ]
arbDomNode n = oneof [ Node <$> arbNodeName <*> resize (n-1) (vector 1),
                       Node <$> arbNodeName <*> resize (n `div` 2) (vector 2) ]

arbNodeName :: Gen String
arbNodeName = elements [ "h1", "h2", "div", "span", "form", "input", "p" ]


size :: DomNode -> Int
size n = 1

prop_hasSize :: DomNode -> Bool
prop_hasSize node = size node == 2
