module GenHtmlDoc where

import Control.Monad.State
import Test.QuickCheck
import Text.PrettyPrint
import Css.CssTypes
import Html.HtmlNode
import Css.GenCss
import Html.GenHtml
import Css.ShowCss
import Html.ShowHtml
import HtmlDoc

instance Arbitrary HtmlDoc where
  arbitrary = genHtml
  shrink = shrinkHtml

genHtml :: Gen HtmlDoc
genHtml = do
  css <- arbitrary
  html <- arbitrary
  return $ htmlDoc css $ addIdsToHtml html

renderHtml :: HtmlDoc -> String
renderHtml (HtmlDoc (css, html)) = render $ ppdoc css html


h :: IO ()
h  = generate genHtml >>= putStrLn . renderHtml

content :: String -> Doc -> Doc
content name doc = vcat [ text ("<" ++ name ++ ">"),
                      nest 2 doc,
                      text ("</" ++ name ++ ">") ]

ppdoc :: [CssRule] -> [HtmlNode] -> Doc
ppdoc css html = content "html" $ hhead css $$ hbody html

hhead css = content "head" (reset $$ content "style" (vcat $ map (text . showcss) css))
hbody html = content "body" (hcat $ map (text . compactHtml) html)

reset = text $ "<style>" ++ cssReset ++ "</style>"

addIdsToHtml :: [HtmlNode] -> [HtmlNode]
addIdsToHtml nodes = fst $ runState (addIds nodes) 0

addIds :: [HtmlNode] -> State Int [HtmlNode]
addIds nodes = mapM addIds' nodes

addIds' :: HtmlNode -> State Int HtmlNode
addIds' n = case n of
  Text _ -> return n
  Node attrs children -> do
    id <- nextId
    newChildren <- addIds children
    return $ Node (setId ("n" ++ show id) attrs) newChildren

nextId :: State Int Int
nextId = do
  id <- get
  put (id + 1)
  return id


shrinkHtml :: HtmlDoc -> [HtmlDoc]
shrinkHtml (HtmlDoc (css, doc)) = [ HtmlDoc (css', doc') | (css', doc') <- shrink (css, doc) ]



cssReset = "html,body,div,span,applet,object,iframe,h1,h2,h3,h4,h5,h6,p,blockquote,pre,a,abbr,acronym,address,big,cite,code,del,dfn,em,img,ins,kbd,q,s,samp,small,strike,strong,sub,sup,tt,var,b,u,i,center,dl,dt,dd,ol,ul,li,fieldset,form,label,legend,table,caption,tbody,tfoot,thead,tr,th,td,article,aside,canvas,details,embed,figure,figcaption,footer,header,hgroup,menu,nav,output,ruby,section,summary,time,mark,audio,video{margin:0;padding:0;border:0;font-size:100%;font:inherit;vertical-align:baseline}article,aside,details,figcaption,figure,footer,header,hgroup,menu,nav,section{display:block}body{line-height:1;height:100%}ol,ul{list-style:none}blockquote,q{quotes:none}blockquote:before,blockquote:after,q:before,q:after{content:'';content:none}table{border-collapse:collapse;border-spacing:0}"
