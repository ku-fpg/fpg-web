{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, LambdaCase, InstanceSigs, FlexibleContexts, TypeFamilies #-}

module KURE where

import Text.XML.HXT.Parser.HtmlParsec
import Text.XML.HXT.DOM.ShowXml
import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.DOM.XmlNode
import Data.Tree.NTree.TypeDefs

import Control.Arrow
import Control.Applicative

import Data.Monoid
import Control.Monad

--import Language.KURE.Walker
import qualified Language.KURE as KURE
import Language.KURE hiding ()

newtype HTML  = HTML XmlTrees       -- list of blocks and/or text

newtype Block = Block XmlTree      -- block with tag and attrs

newtype Text  = Text XmlTree        -- text

newtype Attrs = Attrs XmlTrees     -- attributes for a block

newtype Attr  = Attr XmlTree       -- single attribute

newtype Syntax  = Syntax XmlTree       -- XML/HTML syntax, like <? or <!

newtype Context = Context [String]  -- all the containing nodes, in inside to outside order

data Node
        = HTMLNode      HTML
        | BlockNode     Block
        | TextNode      Text
        | AttrsNode     Attrs
        | AttrNode      Attr
        | SyntaxNode    Syntax
        deriving Show

-----------------------------------------------------------------------------

instance Show HTML where
        show (HTML html) = xshow html

instance Show Block where
        show (Block html) = xshow [html]

instance Show Text where
        show (Text html) = xshow [html]

instance Show Attrs where
        show (Attrs html) = xshow html

instance Show Attr where
        show (Attr html) = xshow [html]

instance Show Syntax where
        show (Syntax syntax) = xshow [syntax]

instance Monoid HTML where
        mempty = HTML []
        mappend (HTML xs) (HTML ys) = HTML $ xs ++ ys

-----------------------------------------------------------------------------
-- KURE specific instances

instance Injection HTML Node where
        inject    = HTMLNode
        project u = do HTMLNode t <- return u
                       return t

instance Injection Block Node where
        inject    = BlockNode
        project u = do BlockNode t <- return u
                       return t

instance Injection Text Node where
        inject    = TextNode
        project u = do TextNode t <- return u
                       return t

instance Injection Attrs Node where
        inject    = AttrsNode
        project u = do AttrsNode t <- return u
                       return t

instance Injection Attr Node where
        inject    = AttrNode
        project u = do AttrNode t <- return u
                       return t

instance Injection Syntax Node where
        inject    = SyntaxNode
        project u = do SyntaxNode t <- return u
                       return t

instance Walker Context Node where
        allR :: forall m . MonadCatch m => Rewrite Context m Node -> Rewrite Context m Node
        allR rr = prefixFailMsg "allR failed: " $
          rewrite $ \ c -> \ case
            HTMLNode  o  -> liftM HTMLNode  $ KURE.apply (htmlT  (extractR rr >>> arr html)
                                                                 (extractR rr >>> arr html)
                                                                 (extractR rr >>> arr html)   $ htmlC) c o
            BlockNode o  -> liftM BlockNode  $ KURE.apply (blockT (extractR rr) (extractR rr) $ blockC) c o
            TextNode  o  -> liftM TextNode   $ return o
            AttrsNode o  -> liftM AttrsNode  $ KURE.apply (attrsT (extractR rr)               $ attrsC) c o
            AttrNode  o  -> liftM AttrNode   $ return o
            SyntaxNode o -> liftM SyntaxNode $ return o -- never processed

class Html a where
        html :: a -> HTML

instance Html Block where
        html (Block b) = HTML [b]

instance Html Text where
        html (Text b) = HTML [b]

instance Html Syntax where
        html (Syntax b) = HTML [b]


-----------------------------------------------------------------------------

htmlT :: (Monad m)
     => Translate Context m Block a             -- used many times
     -> Translate Context m Text a              -- used many times
     -> Translate Context m Syntax a            -- used many times
     -> ([a] -> x)
     -> Translate Context m HTML x
htmlT tr1 tr2 tr3 k = translate $ \ c (HTML ts) -> liftM k $ flip mapM ts $ \ case
                        t@(NTree (XTag {}) _)     -> apply tr1 c (Block t)
                        t@(NTree (XText {}) _)    -> apply tr2 c (Text t)
                        t@(NTree (XCharRef n) _)  -> apply tr2 c (Text t)
                        t@(NTree (XPi {}) _)      -> apply tr3 c (Syntax t)
                        t@(NTree (XDTD {}) _)     -> apply tr3 c (Syntax t)
                        t@(NTree (XCmt {}) _)     -> apply tr3 c (Syntax t)
                        t@(NTree (XError {}) _)   -> apply tr3 c (Syntax t)
                        t -> error $ "not XTag or XText: " ++ take 100 (show t)

htmlC :: [HTML] -> HTML
htmlC = mconcat

blockT :: (Monad m)
     => Translate Context m Attrs a
     -> Translate Context m HTML b
     -> (String -> a -> b -> x)
     -> Translate Context m Block x
blockT tr1 tr2 k = translate $ \ (Context cs) (Block t) ->
        case t of
          NTree (XTag tag attrs) rest
            | namePrefix tag == ""
           && namespaceUri tag == "" -> do
                  let nm = localPart tag
                  let c = Context (nm : cs)
                  attrs' <- apply tr1 c (Attrs attrs)
                  rest'  <- apply tr2 c (HTML rest)
                  return $ k nm attrs' rest'
          _ -> fail "blockT runtime type error"

blockC :: String -> Attrs -> HTML -> Block
blockC nm (Attrs attrs) (HTML rest) = Block (NTree (XTag (mkName nm) attrs) rest)

textT :: (Monad m)
      => (String -> x)
      -> Translate Context m Text x
textT k = translate $ \ c -> \ case
                Text (NTree (XText txt) []) -> return $ k txt
                Text t@(NTree (XCharRef n) []) -> return $ k $ xshow [t]
                _                           -> fail "textT runtime error"

textC :: String -> Text
textC = Text . mkText

attrsT :: (Monad m)
       => Translate Context m Attr a
       -> ([a] -> x)
       -> Translate Context m Attrs x
attrsT tr k = translate $ \ c (Attrs ts) -> liftM k $ flip mapM ts $ \ case
                        t@(NTree (XAttr {}) _) -> apply tr c (Attr t)
                        _                      -> fail "not XTag or XText"

attrsC :: [Attr] -> Attrs
attrsC xs = Attrs [ x | Attr x <- xs ]


attrT :: (Monad m)
      => (String -> String -> x)
      -> Translate Context m Attr x
attrT k = translate $ \ c -> \ case
                Attr (NTree (XAttr nm) [NTree (XText txt) []])
                   | namePrefix nm == ""
                  && namespaceUri nm == "" -> return $ k (localPart nm) txt
                _                          -> fail "textT runtime error"

attrC :: String -> String -> Attr
attrC nm val = Attr $ mkAttr (mkName nm) [mkText val]

attr = attrC

attrs = attrsC

--------------------------------------------------
-- HTML Builders.

block :: String -> [Attr] -> HTML -> HTML
block nm xs inner = HTML [t]
  where Block t = blockC nm (attrsC xs) inner

single :: String -> [Attr] -> HTML
single nm xs = block nm xs mempty

text txt = HTML [t]
  where Text t = textC txt

--------------------------------------------------
-- observers

getAttr :: forall a m g . (MonadCatch m, Injection a g, g ~ Node) => String -> Translate Context m a String
getAttr nm = extractT' $ onetdT $ promoteT' (find >>> joinT)
  where
          find = attrT $ \ nm' val -> if nm' == nm
                                      then return val
                                      else fail $ "getAttr: not" ++ show nm

isTag :: (Monad m) => String -> Translate Context m Block ()
isTag nm = blockT idR idR (\ nm' _ _ -> nm == nm') >>> guardT

getTag :: (Monad m) => Translate Context m Block String
getTag = blockT idR idR (\ nm _ _ -> nm)

--------------------------------------------------
-- common pattern; promote a translation over a block to over

blockToHTML :: Block -> HTML
blockToHTML (Block t) = HTML [t]

extractT' :: (Monad m, Injection a g, g ~ Node) => Translate c m g b -> Translate c m a b
extractT' = extractT

promoteT' :: (Monad m, Injection a g, g ~ Node) => Translate c m a b -> Translate c m g b
promoteT' = promoteT

extractR' :: (Monad m, Injection a g, g ~ Node) => Rewrite c m g -> Rewrite c m a
extractR' = extractR

promoteR' :: (Monad m, Injection a g, g ~ Node) => Rewrite c m a -> Rewrite c m g
promoteR' = promoteR


---------------------------------------

mapHTML :: (Monad m) => Translate Context m Block HTML -> Translate Context m HTML HTML
mapHTML tr = htmlT tr (arr html) (arr html) htmlC

parseHTML :: FilePath -> String -> HTML
parseHTML fileName input = HTML $ parseHtmlDocument fileName input


