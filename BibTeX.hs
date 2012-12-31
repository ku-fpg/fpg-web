module BibTeX where


import Development.Shake hiding (getDirectoryContents)
import Development.Shake.FilePath

import System.Directory hiding (doesFileExist)
import System.Environment
import Control.Monad
import qualified Control.Exception as E
import System.Posix (getSymbolicLinkStatus, isDirectory)
import Control.Arrow
import Control.Applicative hiding ((*>))
import Data.List
import Data.Char
import Debug.Trace
import Control.Monad.IO.Class
import Data.Time.LocalTime
import Data.Time.Format
import System.Locale

import Text.XML.HXT.Parser.HtmlParsec
import Text.XML.HXT.DOM.ShowXml
import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.DOM.XmlNode

import Language.KURE.Walker
import Data.Tree.NTree.TypeDefs
import Control.Concurrent
import Control.Concurrent.MVar

import qualified Language.KURE as KURE
import Language.KURE hiding (apply)

import qualified Text.BibTeX.Entry as B
import qualified Text.BibTeX.Parse as P
import qualified Text.Parsec as Parsec

import Data.Monoid

import Control.Concurrent.STM

import KURE

import Shake


data BibTeX = BibTeX String [(String,String)]
        deriving (Show, Read)

readBibTeX :: String -> IO [(String,BibTeX)]
readBibTeX fileName = do
        txt <- readFile fileName
        let bib = Parsec.runP P.file () fileName txt
        case bib of
          Right bibs -> return [ (B.identifier bib,BibTeX (B.entryType bib) (B.fields bib))
                               | bib <- bibs
                               ]
          Left msg -> fail $ show msg

test = do
        txt <- readFile "data/fpg.bib"
        let bib = Parsec.runP P.file () "data/fpg.bib" txt
        case bib of
          Right bibs -> return bibs

paper_page_dir :: String
paper_page_dir = "Papers"

files_dir :: String
files_dir = "Files"


asciiBibText :: String -> BibTeX -> String
asciiBibText id (BibTeX ty stuff) = unlines $
        ["@" ++ ty ++ "{" ++ id ++ ","] ++
        [ "    " ++ tag ++ " = {" ++ dat ++ "}"
        | (tag,dat) <- stuff
        , tag /= "abstract"
        , head tag /= 'X'
        ] ++
        ["}"]

-- Build textual citatation, with link(s).
buildBibCite :: String -> BibTeX -> HTML
buildBibCite id (BibTeX ty stuff) = HTML $
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

tagToFileName :: String -> String
tagToFileName nm = map fn nm ++ ".html"
  where
          fn ':' = '_'
          fn o   = o