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
--        main2 ["clean"]
        main2 args

main2 ("build":extra) = do

--    build_cmds <- findBuildDirections

    markups <- findBuildTargets "site" "markdown"

    files_to_copy <- fmap concat $ sequence
          [ findBuildTargets "img" "jpg"
          , findBuildTargets "js"  "js"
          , findBuildTargets "css" "css"
          ]

    bib <- readBibTeX "data/fpg.bib"

    let autogen = ["status.html"]

    let prettyPage file dir = htmlPage file dir $
                            wrapTemplateFile "template/page.html" depth
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

    shake shakeOptions { shakeVerbosity = Diagnostic
                       , shakeReport = return "report.html"
                       , shakeThreads = 1
                       } $ do


        addRedirectOracle redirects
        addBibTeXOracle bib

        -- This will make all the target Files / URLs
        chioneRules myURLs

        -- This will make a status file, in the autogen dir
        makeStatus "autogen"

        -- make the content files, using pandoc.
        "_make/contents//*.html" *> \ out -> do
--                liftIO $ prepareDirectory out
                let srcName = dropDirectory1 $ dropDirectory1 $ replaceExtension out ".markdown"
                let input = site_dir </> srcName
                liftIO $ print ("CONTENTS",input)
                need [ input ]
                system' "pandoc" ["-o",out,input]

        "_make/autogen/Sitemap.html" *> \ out -> do
                contents :: [String] <- targetPages
                let trees = genSiteMap "/" (map (("/" ++) . dropDirectory1 . dropDirectory1) contents)
                let sitemap = mkElement (mkName "p") [] trees
                writeFileChanged out $ (xshow [sitemap])

{-
        "_make/autogen/Papers/*.html" *> \ out -> do
                need [ "data/fpg.bib" ]
                bib <- readShakeVar meta_bibtex
                let bib' =  [ (tagToFileName nm,(nm,dat))
                            | (nm,dat) <- bib
                            ]
                case lookup (takeFileName out) bib' of
                  Nothing -> error $ "abort: " ++ show out
                  Just (nm,bb@(BibTeXCitation _ stuff)) -> do
                    traced "paper-out" $ writeFile out $ show
                        $ block "div" [attr "class" "row"]
                          $ block "div" [attr "class" "span8 offset2"]
                            $ htmlC
                              [ block "p" [] (buildBibCite nm bb)
                              , block "h3" [] $ text "Abstract"
                              , block "blockquote" [] $
                                  case lookup "abstract" stuff of
                                    Just txt -> text $ txt
                                    Nothing -> text $ "(no abstract)"
                              , block "h3" [] $ text "BibTeXCitation"
                              , block "pre" [ attr "style" "font-size: 70%"]
                                $ text $ asciiBibText nm bb
                              ]

        "_make/autogen/Publications.html" *> \ out -> do

                need [ "data/fpg.bib" ]
                bib <- readShakeVar meta_bibtex
                let years :: [Int] = reverse $ sort $ nub
                                [ read y
                                | (_,BibTeXCitation _ stuff) <- bib
                                , Just y <- [lookup "year" stuff]
                                ]


                let entries :: HTML
                    entries = mconcat
                        [ block "div" [attr "class" "row"]
                          $ ( block "div" [attr "class" "span1 offset1"]
                              $ block "h3" [attr "style" "margin-top: -7px; border-top: 1px dotted;"]
                                $ text (show year)
                            ) <>
                            (block "div" [attr "class" "span8"]
                             $ block "ul" []
                               $ htmlC [ block "li" [attr "style" "margin-bottom: 2px;"]
                                        $ buildBibCite nm dat
                                      | (nm,dat@(BibTeXCitation _ stuff)) <- bib
                                      , Just y <- [lookup "year" stuff]
                                      ,         read y == year
                                      ]
                            )

                        | year <- years
                        ]


                writeFile' out
                        $ show
                        $ entries
-}

main2 ["clean"] = clean

main2 _ = putStrLn $ unlines
        [ "usage:"
        , "./Main clean          clean up"
        ]



fixTable :: Rewrite Context FPGM Block
fixTable = do
          "table" <- getTag
          extractR' $ anyR $ promoteR' $ do
              ss <- attrsT idR id
              return $ attrsC (attr "class" "table table-bordered table-condensed" : ss)

relativeURL :: Int -> String -> String
relativeURL n ('/':rest) = replaceExtension (local_prefix </> rest) "html"
  where local_prefix = concat (take n $ repeat "../")
relativeURL n other
  | "http://" `isPrefixOf` other
  || "https://" `isPrefixOf` other = other
  | otherwise                      = other

-----------------------------------------------------------
-- Find the name of all the (HTML) targets

{-
findBuildDirections :: [(String,BibTeXCitation)] -> Action [(String,Build)]
findBuildDirections bib = do
        site <- liftIO $ getRecursiveContents "site"
        img  <- liftIO $ getRecursiveContents "img"
        js   <- liftIO $ getRecursiveContents "js"
        css  <- liftIO $ getRecursiveContents "css"
        return $ [ (html_dir </> $ dropDirectory1 $ flip replaceExtension "html" $ file,FromContent)
                 | file <- site
                 , "site//*.markdown" ?== file
                 ] ++
                 [ (html_dir </> file,Copy)
                 | file <- img ++ js ++ css
                 , "img//*.gif" ?== file
                        || "img//*.png" ?== file
                        || "js//*.js" ?== file
                        || "css//*.css" ?== file
                 ] ++
                 [ (html_dir </> file,Redirect)
                 | file <- map fst redirects
                 ] ++
                 [ (html_dir </> file,AutoGenerated)
                 | file <- ["Sitemap.html","Publications.html","admin/Status.html"]
                 ] ++
                 [ (html_dir </> (paper_page_dir </> tagToFileName tag),AutoGenerated)
                 | (tag,_) <- bib
                 ]

html_dir </> :: String -> String
html_dir </> = (build_dir </> "html" </>)
-}
-----------------------------------------------------------

debugR :: (Monad m, Show a) => String -> Int -> Rewrite c m a
debugR msg n = acceptR (\ a -> trace (msg ++ " : " ++ take n (show a)) True)

-- change an embedded URL


-----------------------------------------------------------------------


makeHtmlHtml out contents = do

--                liftIO $ prepareDirectory out
                let srcName = build_dir </> contents </> dropDirectory1 (dropDirectory1 out)
                let tplName = "template" </> "page.html"

                --- now get the relative name for this file.

                let count = length [ () | '/' <- out ]
                let local_prefix nm = concat (take (count - 2) (repeat "../")) ++ nm

--                liftIO $ print(out,count,local_prefix "")


                -- Compute breadcrumbs
                let path0 = [ nm | nm <- splitDirectories $ dropDirectory1 $ dropDirectory1 $  out ]

                -- index itself does not have an entry, but *is* home.
                let path1 = case path0 of
                             ["index.html"] -> []
                             _               -> path0

                need [ srcName , tplName ]

                -- next, the contents
--                liftIO $ print ("srcName",srcName)
                src <- readFile' srcName
                let contents = parseHTML srcName src

                -- next, read the template
                template <- readFile' tplName
                let page0 = parseHTML tplName template

                let normalizeTplURL nm
                        -- should really check for ccs, js, img, etc.
                        | "../" `isPrefixOf` nm = local_prefix (dropDirectory1 nm)
                        | otherwise             = nm
                let relativeURL ('/':rest) = replaceExtension (local_prefix rest) "html"
                    relativeURL other      | "http://" `isPrefixOf` other
                                          || "https://" `isPrefixOf` other = other
                                           | otherwise = other

                let macro :: String -> FPGM HTML
--                    macro "fpg-contents" = return contents
                    macro "fpg-update-time" = do
                            tm <- liftActionFPGM $ liftIO $ getZonedTime
                            let txt = formatTime defaultTimeLocale rfc822DateFormat tm
--                            () <- trace (show ("tm",tm,txt)) $ return ()
                            return (text txt)
                    macro "fpg-sitemap" = do
--                            liftIO $ print "FPG-SITEMAP"
                            liftActionFPGM $ do
                                let fileName = build_dir </> "autogen" </> "Sitemap.html"
                                need [ fileName ]
                                txt <- readFile' fileName
--                                liftIO $ print $ "FPG: " ++ txt
                                return $ parseHTML "Sitemap.html" txt

                    macro nm | match `isPrefixOf` nm  && all isDigit rest = do
                            liftActionFPGM $ do
                                let fileName = build_dir </> "contents" </> "Events.html"
                                need [ fileName ]
                                txt <- readFile' fileName
                                let events = parseHTML "Events.html" txt
                                tm <- liftIO $ getZonedTime
                                let time_txt = formatTime defaultTimeLocale "%Y" tm

                                let spotH2 = do "h3" <- getTag
                                                arr (\ x -> (True,html x))

                                let up :: (Html h) => T h (Bool,HTML)
                                    up = arr html >>> arr (\ x -> (False,x))

                                xs <- applyFPGM' (htmlT (spotH2 <+ up) up up id) events

                                let counts :: [Int]
                                    counts = [ i
                                             | ((True,_),i) <- xs `zip` [0..]
                                             ]

                                let events' = take (head (drop (read rest) counts ++ [0])) (map snd xs)

                                return $ block "div" [attr "class" "fpg-event-list"]
                                       $ mconcat events'

                      where
                        match = "fpg-recently-"
                        rest = drop (length match) nm
                        insert x (xs:xss) = (x : xs) : xss

                    macro _             = fail "macro failure"

                let macroExpand :: Translate Context FPGM Block HTML
                    macroExpand = do
                         tag <- getTag
                         guardMsg (tag == "div" || tag == "span") "wrong tag"
--                         () <- trace ("trace: " ++ show tag) $ return ()
                         cls <- getAttr "class"
--                         () <- trace ("$$$$$$$$$$$$$$$$$ trace: " ++ show (tag,cls)) $ return ()
                         constT $ macro cls


                let fixLandingPage :: Rewrite Context FPGM Node
                    fixLandingPage = promoteR $ do
                          guardMsg (path0 == ["index.html"]) "is not landing page"
                          "body" <- getTag
                          extractR' $ anyR $ promoteR' $ do
                                  ss <- attrsT idR id
                                  return $ attrsC (attr "class" "fpg-landing" : ss)



                let findActive :: R Block
                    findActive = do
                            "li" <- getTag
                            extractT' $ onetdT $ promoteT' $ do
                                "a" <- getTag
                                url <- getAttr "href"
                                guardMsg (url == dropDirectory1 (dropDirectory1 out)) "this link is not active"
                                return ()
                            extractR' $ anyR $ promoteR' $ do
                                  ss <- attrsT idR id
                                  return $ attrsC (attr "class" "active" : ss)


                let tpl_prog :: Rewrite Context FPGM HTML
                    tpl_prog = extractR' (tryR (prunetdR (promoteR $ mapURL normalizeTplURL)))
                           >>> injectHTML srcName "contents"
                           >>> extractR' (tryR (alltdR (tryR (promoteR (anyBlockHTML (macroExpand))))))
                           >>> extractR' (tryR (alltdR (tryR (promoteR (anyBlockHTML (insertTeaser))))))
                           >>> extractR' (tryR (prunetdR (promoteR $ mapURL relativeURL)))
                           >>> extractR' (tryR (alltdR (tryR (promoteR findActive))))
--                           >>> extractR' (tryR (prunetdR fixTable))
                           >>> extractR' (tryR (prunetdR fixLandingPage))

--                page1 <- applyFPGM' (extractR tpl_prog) page0

                templateToHTML tplName (extractR tpl_prog) out

--                traced "out1" $ writeFile out $ show page1



----------------------------------------------------
-- ShakeVar (call them shake vars)


genSiteMap :: String -> [String] -> NTrees XNode
genSiteMap dir files = concat
        [ [ mkElement (mkName "a")
                        [mkAttr (mkName "href") [mkText file],mkAttr (mkName "class") [mkText "label"]]
                        [mkText $ takeFileName $ dropExtension file0]
          , mkText " "
          ]
        | file <- files
        , let file0 = tail file
        , takeExtension file == ".html"
        ]



--          E.Cons {E.entryType :: String,
--            E.identifier :: String,
--            E.fields :: [(String, String)]}

--buildBibPage ::
-------------------------------------------------------


