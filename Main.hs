{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, LambdaCase, InstanceSigs, FlexibleContexts #-}
-- @
--
--      site/ ... name.page
--
--      _make/contents/name.markdown
-- @

import Development.Shake hiding (getDirectoryContents)
import Development.Shake.FilePath

import System.Directory hiding (doesFileExist)
import qualified System.Directory as Directory
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
import Data.Time.Clock
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

--import qualified Text.BibTeX.Entry as B
--import qualified Text.BibTeX.Parse as P
--import qualified Text.Parsec as Parsec

import System.Process
import System.Exit


import Data.Monoid

import Control.Concurrent.STM

import Control.Concurrent.ParallelIO.Local

import Text.HTML.KURE
import Shake
--import BibTeX

import Web.Chione
import Web.Chione.BibTeX

site_dir     = "site"

paper_page_dir = html_dir </> "Papers"

-- tables about the website

redirects :: [(String,String)]
redirects =
        [("Tools.html","Software.html")]

-- The Makefile part

main = do
        args <- getArgs
        main2 args

main2 ("build":extra) = do

    markups <- findBuildTargets "site" "markdown"

    files_to_copy <- fmap concat $ sequence
          [ findBuildTargets "img" "jpg"
          , findBuildTargets "img" "gif"
          , findBuildTargets "js"  "js"
          , findBuildTargets "css" "css"
          ]

    bib <- fmap (fmap (\ cite@(BibTeXCitation _ nm _)-> (tagToFileName nm,cite)))
         $ readBibTeX "data/fpg.bib"

    let autogen = [ "papers" </> nm
                  | (nm,_) <- bib
                  ] ++ if "status" `elem` extra
                       then ["status.html"]
                       else []

    let prettyPage file dir = htmlPage file dir $
                            wrapTemplateFile "template/page.html" depth
                        >>> extractR' (tryR (prunetdR (promoteR (anyBlockHTML (divSpanExpand macro)))))
                        >>> extractR' (tryR (prunetdR (promoteR (anyBlockHTML insertTeaser))))
                        >>> extractR' (tryR (prunetdR (promoteR $ mapURL $ relativeURL depth)))
                        >>> extractR' (tryR (prunetdR $ promoteR fixTable))

          where
                  depth = length $ filter (== '/') file

    let myURLs = [ prettyPage (flip replaceExtension "html" $ dropDirectory1 $ file) "contents"
                 | file <- markups
                 ] ++
                 [ prettyPage file "autogen"
                 | file <- autogen
                 ] ++
                 [ copyPage file
                 | file <- files_to_copy
                 ] ++
                 [ redirectPage file
                 | file <- map fst redirects
                 ]

    print myURLs

    shake shakeOptions { shakeVerbosity = Normal
                       , shakeReport = return "report.html"
                       , shakeThreads = 1
                       } $ do


        addRedirectOracle redirects
        addBibTeXOracle $
                bib

        -- This will make all the target Files / URLs
        chioneRules myURLs

        -- This will make a status file, in the autogen dir
        makeStatus "autogen"

        -- make the content files, using pandoc.
        "_make/contents//*.html" *> \ out -> do
--                liftIO $ prepareDirectory out
                let srcName = dropDirectory1 $ dropDirectory1 $ replaceExtension out ".markdown"
                let input = site_dir </> srcName
                need [ input ]
                system' "pandoc" ["-o",out,input]


        "_make/autogen/papers/*.html" *> \ out -> do
                cite@(BibTeXCitation _ _ stuff) <- getBibTeXCitation (dropDirectory1 (dropDirectory1 (dropDirectory1 out)))
                writeFile' out $ "HELLO: " ++ show cite
                traced "paper-out" $ writeFile out $ show
                        $ block "div" [attr "class" "row"]
                          $ block "div" [attr "class" "span8 offset2"]
                            $ htmlC
                              [ block "p" [] (buildBibCite Nothing cite)
                              , block "h3" [] $ text "Links"
                              , block "h3" [] $ text "Abstract"
                              , block "blockquote" [] $
                                  case lookup "abstract" stuff of
                                    Just txt -> text $ txt
                                    Nothing -> text $ "(no abstract)"
                              , block "h3" [] $ text "BibTeX"
                              , block "pre" [ attr "style" "font-size: 70%"]
                                $ text $ asciiBibText cite
                              ]



main2 ["clean"] = clean

main2 _ = putStrLn $ unlines
        [ "usage:"
        , "./Main clean          clean up"
        , "       build          build pages"
        , "       build status   build pages; report status of pages"
        ]



fixTable :: Rewrite Context FPGM Block
fixTable = do
          "table" <- getTag
          extractR' $ anyR $ promoteR' $ do
              ss <- attrsT idR id
              return $ attrsC (attr "class" "table table-bordered table-condensed" : ss)


macro :: String -> FPGM HTML
macro "fpg-update-time" = do
    tm <- liftActionFPGM $ liftIO $ getZonedTime
    let txt = formatTime defaultTimeLocale rfc822DateFormat tm
    () <- trace (show ("tm",tm,txt)) $ return ()
    return (text txt)
macro nm = fail $ "failed macro" ++ nm

