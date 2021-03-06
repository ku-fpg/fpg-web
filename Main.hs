{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, LambdaCase, InstanceSigs, FlexibleContexts #-}
-- @
--
--      site/ ... name.page
--
--      _make/contents/name.md
-- @

import Development.Shake hiding (getDirectoryContents)
import Development.Shake.FilePath

import System.Directory hiding (doesFileExist)
import qualified System.Directory as Directory
import System.Environment
import Control.Monad
import qualified Control.Exception as E
import Control.Arrow
import Control.Applicative hiding ((*>))
import Data.List
import Data.Char
import Debug.Trace
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Format
import System.Locale

import Language.KURE.Walker
import Language.KURE.Debug

import qualified Language.KURE as KURE
import Language.KURE hiding (apply)

--import System.Process
import System.Exit

import Data.Monoid

import Text.HTML.KURE

import Web.Chione
import Web.Chione.BibTeX

site_url     = "http://www.ittc.ku.edu/csdl/fpg"

-- TODO: this needs generalized
target_dir   = "andygill@drumchapel:/projects/csdl/htdocs/fpg"

site_dir     = "site"

bibtex_dir   = build_dir </> "bibtex"

paper_page_dir = html_dir </> "Papers"

-- tables about the website

-- Note we also use .htaccess to provide legacy links
redirects :: [(String,String)]
redirects =
        [("tools.html","software.html")]

-- The Makefile part

main = do
        args <- getArgs
        main2 args

main2 ("build":extra) = do

    markups <- findBuildTargets "site" "md"
    raw_html <- findBuildTargets "site" "html"

    files_to_copy <- fmap concat $ sequence
          [ findBuildTargets "img"   "jpg"
          , findBuildTargets "img"   "gif"
          , findBuildTargets "img"   "png"
          , findBuildTargets "js"    "js"
          , findBuildTargets "css"   "css"
          , findBuildTargets "files" "*"
          , return [".htaccess"]
          ]

    bib <- fmap (fmap (\ cite -> (tagToFileName $ getBibTexCitationTag cite,cite)))
         $ readBibTeX "data/fpg.bib"


    let autogen = [ "papers" </> replaceExtension nm ".html"
                  | (nm,cite) <- bib
                  , not (isXtra cite)
                  ] ++
                  [ "publications.html"
                  ] ++
                  [ "status.html" | "status" `elem` extra ]

    let prettyPage file dir = htmlPage file dir $ idR
                        >>> addDefaultWidth
                        >>> wrapTemplateFile ("template" </> templ) depth
                        >>> expandMacros
                        >>> insertTeasers
                        >>> fixURLs depth
                        >>> fixTables
                        >>> fixActiveLinks file

          where
                  depth = length $ filter (== '/') file
                  templ = if file `elem` ["status.html","sigplan/status.html"]
                          then "full.html"
                          else "page.html"

    let myURLs = [ prettyPage (flip replaceExtension "html" $ dropDirectory1 $ file) "contents"
                 | file <- markups
                 ] ++
                 [ prettyPage file "autogen"
                 | file <- autogen
                 ] ++
                 [ prettyPage (dropDirectory1 file) "raw"
                 | file <- raw_html
                 ] ++
                 [ copyPage file
                 | file <- files_to_copy
                 ] ++
                 [ redirectPage file
                 | file <- map fst redirects
                 ]

--    print myURLs

    shake shakeOptions { shakeVerbosity = Normal
                       , shakeReport = return "report.html"
                       , shakeThreads = 4
                       } $ do


        addRedirectOracle redirects
        addBibTeXOracle bib

        -- This will make all the target Files / URLs
        chioneRules myURLs


        when ("publish" `elem` extra) $ action $ do
--                need [html_dir </> "status.html"]
		need [html_dir </> ".htaccess"]
                system' "rsync" ["-vaz",html_dir ++ "/",target_dir]

        -- This will make a status file, in the autogen dir
        makeStatus site_url "autogen"


        -- make the content files, using pandoc.
        "_make/raw//*.html" *> \ out -> do
                let srcName = dropDirectory1 $ dropDirectory1 out
                let input = site_dir </> srcName
                copyFile' input out

        -- make the content files, using pandoc.
        "_make/contents//*.html" *> \ out -> do
--                liftIO $ prepareDirectory out
                let srcName = dropDirectory1 $ dropDirectory1 $ replaceExtension out "md"
                let input = site_dir </> srcName
                need [ input ]
                system' "pandoc" ["-o",out,input]

-----------------------------------------------------------

        "_make/bibtex/*.bib" *> \ out -> do
                cite <- getBibTeXCitation (dropExtension (dropDirectory1 (dropDirectory1 out)))
                writeFile' out $ asciiBibText cite

        "_make/bibtex/*.abstract" *> \ out -> do
                cite <- getBibTeXCitation (dropExtension (dropDirectory1 (dropDirectory1 out)))
                case lookupBibTexCitation "abstract" cite of
                   Just abs_txt -> writeFile' out abs_txt
                   Nothing -> writeFile' out "No Abstract in BiBTeX"
		   		 -- TODO: use  return ()

        "_make/bibtex/*.aux" *> \ out -> do
                cite <- getBibTeXCitation (dropExtension (dropDirectory1 (dropDirectory1 out)))
                writeFile' out $ unlines
                        [ "% generated by Chione"
                        , "\\bibstyle{abbrvnat}"
                        , "\\citation{" ++ getBibTexCitationTag cite ++ "}"
                        , "\\bibdata{" ++ dropExtension (takeFileName out) ++ "}"
                        ]

        "_make/bibtex/*.bbl" *> \ out -> do
                need [ replaceExtension out ".bib"
                     , replaceExtension out ".aux"
                        -- TODO: also the bst file???
                     ]
                systemCwd (dropFileName out) "bibtex" [dropExtension (takeFileName out)]

        "_make/bibtex/*.bbl-short" *> \ out -> do
                txt <- readFile' (replaceExtension out "bbl")
                let macros xs | "\\doi{" `isPrefixOf` xs = "doi: \\texttt{" ++ macros (drop 5 xs)
                    macros (x:xs) = x : macros xs
                    macros [] = []
                writeFile' out $ unlines
                               $ map macros
                               $ takeWhile (not . all isSpace)                  -- take until first blank line
                               $ dropWhile (\ x -> length (takeWhile isSpace x) > 0)
                               $ tail                                           -- and the bibitem line
                               $ dropWhile (not . ("\\bibitem" `isPrefixOf`))   -- drop preamble
                               $ lines
                               $ txt

        "_make/bibtex/*.html-citation" *> \ out -> do
                need [ replaceExtension out "bbl-short"
                     ]
                -- outputs single paragraph
                system' "pandoc" ["-f","latex",
                                  "-t", "html",
                                  replaceExtension out "bbl-short",
                                  "-o", out ]

        "_make/bibtex/*.html-abstract" *> \ out -> do
                need [ replaceExtension out "abstract"
                     ]
                system' "pandoc" ["-f","latex",
                                 "-t", "html",
                                 replaceExtension out "abstract",
                                 "-o", out ]



 -----------------------------------------------------------

        "_make/autogen/papers/*.html" *> \ out -> do
                let name = dropExtension (dropDirectory1 (dropDirectory1 (dropDirectory1 out)))
                cite <- getBibTeXCitation name
                txt <- readFile'  $ "_make/bibtex" </> replaceExtension name "html-citation"
--                liftIO $ print ("TXT",txt)
                let html_cite0 = parseHTML "<internal>" txt

                txt <- readFile'  $ "_make/bibtex" </> replaceExtension name "html-abstract"
                let html_abstract = parseHTML "<internal>" txt

                let badge = element "span" [attr "class" $ "badge badge-info"] . text

                let ul [] = mempty
                    ul xs = mconcat
                          [ element "h3" [] $ text "Links"
                          , element "ul" [] $ mconcat xs
                          ]

                traced "paper-out" $ writeFile out $ show
                        $ element "div" [attr "class" "row"]
                          $ element "div" [attr "class" "span8 offset1"]
                            $ htmlC
                              [ element "div" [attr "class" "well"]
                                $ html_cite0
                              , ul [ element "li" [] $
                                     mconcat [ element "a" [ attr "href" url ] $ text url
                                             , if "http://doi.acm.org/" `isPrefixOf` url
                                             then text " " <> badge "ACM DL"
                                          else if site_url `isPrefixOf` url
                                             then text " " <> badge ("local "++ takeExtension url)
                                             else mempty
                                             ]
                                   | Just url <- [ lookupBibTexCitation f cite | f <- ["url","xurl"] ]

                                   ]
                              , element "h3" [] $ text "Abstract"
                              , html_abstract
                              , element "h3" [] $ text "BibTeX"
                              , element "pre" [ attr "style" "font-size: 70%"]
                                $ text $ asciiBibText $ filterBibTexCitation
                                                      (\ tag -> tag /= "abstract" && head tag /= 'X')
                                                      $ cite
                              ]

        "_make/autogen/publications.html" *> \ out -> do

                citations :: [(String,Int,HTML)] <- fmap concat $ sequence
                        [ do cite <- getBibTeXCitation nm
                             let year = case lookupBibTexCitation "year" cite of
                                          Just n | all isDigit n -> read n
                                          Nothing -> 0
                             html_txt0 <- citation nm
                             let tr = extractR' $ allbuR $ tryR $ promoteR $ anyElementHTML $ do
                                        -- if the tag is an anchor then just return the inside text
                                        "a" <- getTag
                                        getInner
                             html_txt1 <- applyFPGM tr html_txt0
                             return [(nm,year,html_txt1) | not (isXtra cite) ]
                        | (nm,_) <- bib
                        ]

                let years :: [Int] = reverse $ sort $ nub [ y | (_,y,_) <- citations ]

--                liftIO $ print $ years

                writeFile' out $ show $ mconcat
                        [ element "div" [attr "class" "row"]
                          $ ( element "div" [attr "class" "span1 offset1"]
                              $ element "h3" [attr "style" "margin-top: -7px; border-top: 1px dotted;"]
                                $ text (show year)
                            ) <>
                            (element "div" [attr "class" "span6"]
                             $ element "ul" []
                               $ htmlC [ element "li" [attr "style" "margin-bottom: 2px;"]
                                         $ element "div" [ attr "class" "cite-link" ]
                                         $ element "a" [ attr "href" ("papers" </> replaceExtension nm "html") ]
                                         $ html_txt
                                       | (nm,y,html_txt) <- citations
                                       , y == year
                                       ]
                            )
                        | year <- years
                        ]


main2 ["clean"] = clean

main2 _ = putStrLn $ unlines
        [ "usage:"
        , "./Main clean                    clean up"
        , "       build [status] [publish] build pages; report status; publish"
        ]

isXtra :: BibTeXCitation -> Bool
isXtra cite = case lookupBibTexCitation "xtra" cite of
                Nothing -> False
                _       -> True

citation :: String -> Action HTML
citation nm = do
        txt <- readFile' (bibtex_dir </> addExtension nm  ".html-citation")
        let html_txt0 = parseHTML nm txt
        let tr = extractR' $ allbuR $ tryR $ promoteR $ anyElementHTML $ do
                        -- if the tag is "p", then just return the inside
                        "p" <- getTag
                        getInner

        applyFPGM tr html_txt0


-- Here are the pretty-fication passes

expandMacros :: R HTML
expandMacros = extractR' $ tryR $ prunetdR $ promoteR $ anyElementHTML $ divSpanExpand macro
 where
         macro :: String -> FPGM HTML
         macro "fpg-update-time" = do
                 tm <- liftActionFPGM $ liftIO $ getZonedTime
                 let txt = formatTime defaultTimeLocale rfc822DateFormat tm
                 () <- trace (show ("tm",tm,txt)) $ return ()
                 return (text txt)
         macro xs | "cite " `isPrefixOf` xs = do
                let ["cite",wd] = words xs
--                liftActionFPGM $ liftIO $ print wds
                x <- liftActionFPGM $ citation $ tagToFileName $ wd
                cite <- liftActionFPGM $ getBibTeXCitation $ tagToFileName wd
                liftActionFPGM $ liftIO $ print cite
                return $ element "div" [attr "style" "cite-link"]
                       $ (if isXtra cite
                          then id
                          else element "a" [attr "href" ("/papers/" ++ tagToFileName wd)])
                       $ x
         macro nm = fail $ "failed macro" ++ nm

insertTeasers :: R HTML
insertTeasers = extractR' $ tryR $ prunetdR $ promoteR $ anyElementHTML $ insertTeaser

fixURLs :: Int -> R HTML
fixURLs depth = extractR' $ tryR $ prunetdR $ promoteR $ mapURL $ (relativeURL depth . selfReference)

selfReference :: String -> String
selfReference nm | (site_url ++ "/") ==  nm          = "/index"
                 | (site_url ++ "/") `isPrefixOf` nm = drop (length site_url) nm
                 | otherwise                         = nm

fixTables :: R HTML
fixTables = extractR' $ tryR $ prunetdR $ promoteR $ do
                  "table" <- getTag
                  extractR' $ anyR $ promoteR' $ do
                          ss <- attrsT idR id
                          return $ attrsC (attr "class" "table table-bordered table-condensed" : ss)



fixActiveLinks :: String -> R HTML
fixActiveLinks file = do
        tryR $ extractR' $ anytdR (promoteR is_nav_ul >>> anytdR (promoteR new_li >>> anytdR (promoteR active_link)))

   where
        is_nav_ul :: R Element
        is_nav_ul = do
           "ul" <- getTag
           cls <- getAttr "class"
           if "nav" `elem` words cls then idR else fail "not class nav"

        new_li :: R Element
        new_li = do
           "li" <- getTag
           setAttr "class" $ appendClass "active"

        active_link :: R Element
        active_link = do
                "a" <- getTag
                url <- getAttr "href"
                if (takeDirectory file </> url) == file then idR else do
                        fail "not fount active URL"

-- find out if "row" has been used,
addDefaultWidth :: R HTML
addDefaultWidth = has_row <+ add_row
   where
     has_row = extractR' $ anytdR $ promoteR $ do
        "div" <- getTag
        cls <- getAttr "class"
        if any ("span" `isPrefixOf`) $ words cls then idR else fail "not span div"

     add_row = promoteR $ do
             h <- idR
             return $ element "div" [attr "class" "row"]
                    $ element "div" [attr "class" "span8 offset1"]
                    $ h

-------------------------------------------------------------------------------------------------
-- To roll into std library
consAttr :: Attr -> R Attrs
consAttr x = attrsT idR (attrsC . (x :))

appendClass :: String -> Maybe String -> String
appendClass cls Nothing = cls
appendClass cls (Just rest) = cls ++ " " ++ rest

setAttr :: (MonadCatch m) => String -> (Maybe String -> String) -> Rewrite Context m Element
setAttr nm update = change <+ append
  where
     change = do
        _ <- getAttr nm -- needs to be here here
        elementT (attrsT (attrT $ \ n v -> attrC n (if n == nm
                                                    then update (Just v)
                                                    else v))
                                         $ attrsC) idR $ elementC
        -- otherwise, add the new tag
     append = elementT (attrsT idR (attrsC . (attrC nm (update Nothing) :))) idR $ elementC
