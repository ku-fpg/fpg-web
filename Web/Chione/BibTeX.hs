{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, DeriveDataTypeable, MultiParamTypeClasses, ScopedTypeVariables, LambdaCase, InstanceSigs, FlexibleContexts #-}

module Web.Chione.BibTeX where

import qualified Text.BibTeX.Entry as B
import qualified Text.BibTeX.Parse as P
import qualified Text.BibTeX.Format as F
import qualified Text.Parsec as Parsec


import Data.Monoid
import Shake
import Text.HTML.KURE
import Data.Hashable
import Data.Binary

import Development.Shake hiding (getDirectoryContents)
import Development.Shake.FilePath
import Development.Shake.Classes
import Control.DeepSeq

---------------------------------

newtype FindBibTeX = FindBibTeX String deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

addBibTeXOracle :: [(String,BibTeXCitation)] -> Rules ()
addBibTeXOracle db = return ()

findBibTeXCitation :: String -> Action BibTeXCitation
findBibTeXCitation = undefined

-----------------------------------

data BibTeXCitation = BibTeXCitation String String [(String,String)] deriving (Show,Read,Typeable,Eq,Ord)

instance Hashable BibTeXCitation where
        hashWithSalt s (BibTeXCitation a b cs) = hashWithSalt s (a,b,cs)

instance Binary BibTeXCitation where
  put (BibTeXCitation a b cs) = put a >> put b >> put cs
  get = do a <- get
           b <- get
           cs <- get
           return $ BibTeXCitation a b cs

instance NFData BibTeXCitation where
  rnf (BibTeXCitation a b cs) = rnf (a,b,cs)

readBibTeX :: String -> IO [(String,BibTeXCitation)]
readBibTeX fileName = do
        txt <- readFile fileName
        let bib = Parsec.runP P.file () fileName txt
        case bib of
          Right bibs -> return [ (B.identifier bib,BibTeXCitation (B.entryType bib)
                                                                  (B.identifier bib)
                                                                  (B.fields bib))
                               | bib <- bibs
                               ]
          Left msg -> fail $ show msg

asciiBibText :: BibTeXCitation -> String
asciiBibText (BibTeXCitation a b cs) = F.entry (B.Cons a b cs)

-- Build textual citatation, with link(s).
buildBibCite :: BibTeXCitation -> HTML
buildBibCite citation = text "{{{CITATION}}}"

{-
mconcat[]
        HTML $
        [ mkText $ names ++ ", &#8220;"
        , mkElement (mkName "strong")
                []
                [mkText title]
        , mkText $ ",&#8221; "
             ++ inside
             ++ publisher
             ++ location
             ++ date ++ "."
        ] ++
        [ mkText " "
        , mkElement (mkName "a")
                [ mkAttr (mkName "href") [mkText $ "/" ++ paper_page_dir ++ "/" ++ tagToFileName id]
                , mkAttr (mkName "class") [mkText "label"]
                ]
                [mkText $ "Details"]
        ] ++ concat
        [ [ mkText " "
          , mkElement (mkName "a")
                [ mkAttr (mkName "href") [mkText $ url]
                , mkAttr (mkName "class") [mkText "label"]
                ]
                [mkText $ "Download " ++ takeExtension url]
          ]
        | Just url <- return $ lookup "url" stuff
        ]
  where
          names = case lookup "author" stuff of
                    Just n -> n
                    Nothing -> case lookup "editor" stuff of
                                  Just n -> n ++ " (editors)"
                                  Nothing -> error "X"

          title = case lookup "title" stuff of
                    Just n -> n

          inside = case (lookup "booktitle" stuff) of
                    Just m -> m ++ ", "
                    Nothing -> ""

          publisher = case (lookup "publisher" stuff) of
                    Just m -> m ++ ", "
                    Nothing -> ""

          location = case (lookup "location" stuff) of
                    Just m -> m ++ ", "
                    Nothing -> ""

          note = case (lookup "note" stuff) of
                    Just m -> m ++ ", "
                    Nothing -> ""


          date = case (lookup "month" stuff,lookup "year" stuff) of
                   (Nothing,Just y) -> y
                   (Just m,Just y) -> m ++ " " ++ y
                   _ -> ""
-}

tagToFileName :: String -> String
tagToFileName nm = concatMap fn nm ++ ".html"
  where
          fn ':' = "_"
          fn o   = [o]

