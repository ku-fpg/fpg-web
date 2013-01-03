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

import qualified Text.BibTeX.Entry as B
import qualified Text.BibTeX.Parse as P
import qualified Text.Parsec as Parsec

import System.Process
import System.Exit


import Data.Monoid

import Control.Concurrent.STM

import Control.Concurrent.ParallelIO.Local

import Text.HTML.KURE
import Shake
import BibTeX

import Web.Chione

site_dir     = "site"
build_dir    = "_make"

all_src_files = build_dir </> "all_src_files" -- DEAD


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

    meta_contents :: ShakeVar [(String,Build)]  <- newShakeVar "_make/meta/contents.txt"
    meta_redirect :: ShakeVar [(String,String)] <- newShakeVar "_make/meta/redirect.txt"
    meta_bibtex   :: ShakeVar [(String,BibTeX)] <- newShakeVar "_make/meta/bibtex.txt"

    shake shakeOptions { shakeVerbosity = Loud
                       , shakeReport = return "report.html"
                       , shakeThreads = 4
                       } $ do

        meta_bibtex !> do
                alwaysRerun     -- because we are reading the filesystem.
                need ["data/fpg.bib"]
                liftIO $ readBibTeX "data/fpg.bib"

        -- Start by compiling the list of things that
        -- are needed for the final website.
        meta_contents !> do
                alwaysRerun     -- because we are reading the filesystem.
                bib <- readShakeVar meta_bibtex
                findBuildDirections bib

        meta_redirect !> do
                alwaysRerun     -- because this is part of the build
                return redirects

        -- make the content files, using pandoc.
        "_make/contents//*.html" *> \ out -> do
--                liftIO $ prepareDirectory out
                let srcName = dropDirectory1 $ dropDirectory1 $ replaceExtension out ".markdown"
                let input = site_dir </> srcName
                need [ input ]
                system' "pandoc" ["-o",out,input]

        "_make/autogen/Sitemap.html" *> \ out -> do
                contents :: [(String,Build)] <- readShakeVar meta_contents
                let trees = genSiteMap "/" (map (("/" ++) . dropDirectory1 . dropDirectory1 . fst) contents)
                let sitemap = mkElement (mkName "p") [] trees
                writeFileChanged out $ (xshow [sitemap])

        "_make/autogen/admin/Status.html" *> \ out -> do
                contents :: [(String,Build)] <- readShakeVar meta_contents
                status <- generateStatus contents
                writeFileChanged out $ show $ status

        "_make/autogen/Papers/*.html" *> \ out -> do
                need [ "data/fpg.bib" ]
                bib <- readShakeVar meta_bibtex
                let bib' =  [ (tagToFileName nm,(nm,dat))
                            | (nm,dat) <- bib
                            ]
                case lookup (takeFileName out) bib' of
                  Nothing -> error $ "abort: " ++ show out
                  Just (nm,bb@(BibTeX _ stuff)) -> do
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
                              , block "h3" [] $ text "BibTeX"
                              , block "pre" [ attr "style" "font-size: 70%"]
                                $ text $ asciiBibText nm bb
                              ]

        "_make/autogen/Publications.html" *> \ out -> do

                need [ "data/fpg.bib" ]
                bib <- readShakeVar meta_bibtex
                let years :: [Int] = reverse $ sort $ nub
                                [ read y
                                | (_,BibTeX _ stuff) <- bib
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
                                      | (nm,dat@(BibTeX _ stuff)) <- bib
                                      , Just y <- [lookup "year" stuff]
                                      ,         read y == year
                                      ]
                            )

                        | year <- years
                        ]


                writeFile' out
                        $ show
                        $ entries

        -- Finally, building visable results, all in the html sub-directory
        ("_make/html//*") *> \ out -> do
                build_cmds <- readShakeVar meta_contents
                case lookup out build_cmds of
                  Just Copy        -> do
                        let src = dropDirectory1 $ dropDirectory1 $ out
                        copyFile' src out
                  Just FromContent -> makeHtmlHtml out "contents"
                  Just Redirect    -> do
                        redirects <- readShakeVar meta_redirect
                        case lookup (dropDirectory1 $ dropDirectory1 $ out) redirects of
                          Just target -> makeHtmlRedirect out target
                  Just AutoGenerated ->  makeHtmlHtml out "autogen"

                  Just other       -> error $ show other
                  Nothing          -> error $ "can not find command to build " ++ show out

        -- Require the final html files in place
        action $ do
                -- Create work for yourself
                let work = ---["_make/autogen/Publications.html"
--                           ,"_make/html/Publications.html"
--                           , "_make/autogen/Papers/Gill_09_KansasLava.html"
--                           ]
                        ["_make/html/admin/Status.html","_make/autogen/admin/Status.html"]

--                      []
--                b <- doesFileExist "_make/autogen/Papers/Framer_12_HERMITinMachine.html"
                sequence_ [ do
                        b <- doesFileExist f
                        liftIO $ if b then removeFile f
                                 else return () | f <- work ]

                build_cmds <- readShakeVar meta_contents
                need $ filter (not . isAdminFile) $ map fst $ build_cmds

                -- after finish building
                when ("upload" `elem` extra) $ do
-- rsync -avz _make/html/ andygill@drumchapel:/projects/csdl/htdocs/fpg-new
                        system' "rsync"
                                [ "-avz"
                                , "--exclude=/admin"
                                , "_make/html/"
                                , "andygill@drumchapel:/projects/csdl/htdocs/fpg-new"]

                        liftIO $ putStrLn "Uploaded website"

                when ("status" `elem` extra) $ do
                        need ["_make/html/admin/Status.html"]
                        return ()


main2 ["clean"] = do
        b <- doesDirectoryExist build_dir
        when b $ do
           removeDirectoryRecursive build_dir
        return ()

main2 _ = putStrLn $ unlines
        [ "usage:"
        , "./Main clean          clean up"
        ]


-----------------------------------------------------------
-- Find the name of all the (HTML) targets


findBuildDirections :: [(String,BibTeX)] -> Action [(String,Build)]
findBuildDirections bib = do
        site <- liftIO $ getRecursiveContents "site"
        img  <- liftIO $ getRecursiveContents "img"
        js   <- liftIO $ getRecursiveContents "js"
        css  <- liftIO $ getRecursiveContents "css"
        return $ [ (html_prefixed $ dropDirectory1 $ flip replaceExtension "html" $ file,FromContent)
                 | file <- site
                 , "site//*.markdown" ?== file
                 ] ++
                 [ (html_prefixed file,Copy)
                 | file <- img ++ js ++ css
                 , "img//*.gif" ?== file
                        || "img//*.png" ?== file
                        || "js//*.js" ?== file
                        || "css//*.css" ?== file
                 ] ++
                 [ (html_prefixed file,Redirect)
                 | file <- map fst redirects
                 ] ++
                 [ (html_prefixed file,AutoGenerated)
                 | file <- ["Sitemap.html","Publications.html","admin/Status.html"]
                 ] ++
                 [ (html_prefixed (paper_page_dir </> tagToFileName tag),AutoGenerated)
                 | (tag,_) <- bib
                 ]

html_prefixed :: String -> String
html_prefixed = (build_dir </> "html" </>)

-----------------------------------------------------------


-- From RWH, first edition, with handle from Johann Giwer.
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = E.handle (\ E.SomeException {} -> return []) $ do       -- 5
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    s <- getSymbolicLinkStatus path
    if isDirectory s
      then getRecursiveContents path
      else return [path]
  return (concat paths)



-----------------------------------------------------------

-- Prepare a directory for a target file
prepareDirectory :: FilePath -> IO ()
prepareDirectory = createDirectoryIfMissing True . takeDirectory

-----------------------------------------------------------

-----------------------------------------------------------

debugR :: (Monad m, Show a) => String -> Rewrite c m a
debugR msg = acceptR (\ a -> trace (msg ++ " : " ++ take 100 (show a)) True)

-- change an embedded URL

mapURL' :: (Monad m) => (String -> String) -> Rewrite Context m Node
mapURL' f = promoteR $ do
                (nm,val) <- attrT (,)
                cxt@(Context (c:_)) <- contextT
                tag <- KURE.apply getTag cxt c
                case (nm,[tag]) of
                   ("href","a":_)     -> return $ attrC nm $ f val
                   ("href","link":_)  -> return $ attrC nm $ f val
                   ("src","script":_) -> return $ attrC nm $ f val
                   ("src","img":_)    -> return $ attrC nm $ f val
                   _                  -> fail "no correct context"


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
                    macro "fpg-contents" = return contents
                    macro "fpg-update-time" = do
                            tm <- liftIO $ getZonedTime
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

                let fixTable :: Rewrite Context FPGM Node
                    fixTable = promoteR $ do
                          "table" <- getTag
                          extractR' $ anyR $ promoteR' $ do
                                  ss <- attrsT idR id
                                  return $ attrsC (attr "class" "table table-bordered table-condensed" : ss)

                let fixLandingPage :: Rewrite Context FPGM Node
                    fixLandingPage = promoteR $ do
                          guardMsg (path0 == ["index.html"]) "is not landing page"
                          "body" <- getTag
                          extractR' $ anyR $ promoteR' $ do
                                  ss <- attrsT idR id
                                  return $ attrsC (attr "class" "fpg-landing" : ss)

                let findTeaser :: T Block HTML
                    findTeaser = do
                            "div" <- getTag
                            "teaser" <- getAttr "class"
                            getInner

                let insertTeaser :: T Block HTML
                    insertTeaser = do
                            "a"       <- getTag
                            "teaser"  <- getAttr "class"
                            ('/':url) <- getAttr "href"
                            inside    <- getInner

--                            contextfreeT $ \ _ -> liftActionFPGM $ do
--                                need [ "

                            let sub_content = build_dir </> "contents" </> replaceExtension url "html"

                            inside_content <- contextfreeT $ \ _ -> liftActionFPGM $ do
                                    need [ sub_content ]
                                    sub_txt <- readFile' sub_content
                                    let sub_html = parseHTML sub_content sub_txt
                                    applyFPGM' (extractT' (onetdT (promoteT findTeaser))
                                                <+ return (text ("Can not find teaser in " ++ sub_content)))
                                         sub_html

--                            () <- trace (show ("url",inside_content)) $ return ()

                            return $ mconcat [ inside_content
                                             , block "a" [ attr "href" ('/':url)
                                                         , attr "class" "label"
                                                         ]
                                                         inside
                                             ]


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


                let tpl_prog :: Rewrite Context FPGM Node
                    tpl_prog = tryR (prunetdR (mapURL' normalizeTplURL))
                           >>> tryR (alltdR (tryR (promoteR (concatMapHTML (macroExpand <+ arr html)))))
                           >>> tryR (alltdR (tryR (promoteR (concatMapHTML (insertTeaser <+ arr html)))))
                           >>> tryR (prunetdR (mapURL' relativeURL))
                           >>> tryR (alltdR (tryR (promoteR findActive)))
                           >>> tryR (prunetdR fixTable)
                           >>> tryR (prunetdR fixLandingPage)

                page1 <- applyFPGM' (extractR tpl_prog) page0

                traced "out1" $ writeFile out $ show page1


makeHtmlRedirect :: String -> String -> Action ()
makeHtmlRedirect out target = do
        writeFile' out $ "<meta http-equiv=\"Refresh\" content=\"0; url=Research.html\">\n"
  where

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

data LinkData a = LinkData
        { ld_pageName :: String
        , ld_localURLs :: a
        , ld_remoteURLs :: a
        }
        deriving Show

instance Functor LinkData where
        fmap f (LinkData n a b) = LinkData n (f a) (f b)

isAdminFile :: String -> Bool
isAdminFile nm = takeDirectory (dropDirectory1 (dropDirectory1 nm)) == "admin"

data URLResponse = URLResponse [Int] Int deriving Show

getURLResponse :: String -> IO URLResponse
getURLResponse url = do
        tm1 <- getCurrentTime
        (res,out,err) <- readProcessWithExitCode "curl"
                                ["curl","-L","-m","5","-s","--head",url]
                                ""
        tm2 <- getCurrentTime
        let code = concat
               $ map (\ case
                  ("HTTP/1.1":n:_) | all isDigit n  -> [read n :: Int]
                  _                                 -> [])
               $ map words
               $  lines
               $ filter (/= '\r')
               $ out
        putStrLn $        "examining " ++ url ++ " ==> " ++ show code
        return $ URLResponse code (floor (diffUTCTime tm2 tm1 * 1000))

generateStatus :: [(String, Build)] -> Action HTML
generateStatus inp = do
        let files = [ nm0
                    | (nm0,_) <- inp
                    , not (isAdminFile nm0)
                    , "//*.html" ?== nm0
                    ]

        links <- mapM findLinks files

        good_local_links <- liftM concat $ sequence
                         [ do b <- liftIO $ Directory.doesFileExist $ (build_dir </> "html" </> file)
                              if b then return [file]
                                   else return []
                         | file <- nub (concatMap ld_localURLs links)
                         ]

-- sh -c 'curl -m 1 -s --head http://www.chalmers.se/cse/EN/people/persson-anders || echo ""'
-- -L <= redirect automatically
{-
        let classify (x:xs) = case words x of
                    ("HTTP/1.1":n:_) | all isDigit n -> classifyCode (read n) xs
                    _                                -> []
            classify _             = []

            classifyCode :: Int -> [String] -> String
            classifyCode n xs | n >= 300 && n < 400 = if again == unknown
                                                      then show n
                                                      else again
                  where again = classify xs

            classifyCode n _ = show n
-}

        let fake = False
        external_links <- liftIO $ withPool 32
                $ \ pool -> parallelInterleaved pool
                         [ do resp <- getURLResponse url
                              putStrLn $ "examining " ++ url ++ " ==> " ++ show resp
                              return (url,resp)
                         | url <- take 500 $ nub (concatMap ld_remoteURLs links)
                         ]

        liftIO$ print $ external_links

{-
   curl -s --head http://www.haskell.org/
HTTP/1.1 307 Temporary Redirect
Date: Wed, 02 Jan 2013 02:51:59 GMT
Server: Apache/2.2.9 (Debian) PHP/5.2.6-1+lenny13 with Suhosin-Patch
Location: http://www.haskell.org/haskellwiki/Haskell
Vary: Accept-Encoding
Content-Type: text/html; charset=iso-8859-1

orange:fpg-web andy$ curl -s --head http://www.haskell.org/
-}

        let goodLinkCode :: URLResponse -> Bool
            goodLinkCode (URLResponse [] _) = False
            goodLinkCode (URLResponse xs _) = last xs == 200

        let findBadLinks :: LinkData [String] -> LinkData [String]
            findBadLinks link = link
                { ld_localURLs    = filter (`notElem` good_local_links) $ ld_localURLs link
                , ld_remoteURLs = filter (\ url -> case lookup url external_links of
                                                       Nothing -> error "should never happen! (all links looked at)"
                                                       Just resp -> not (goodLinkCode resp))
                                $ ld_remoteURLs link
                }

            markupCount :: [a] -> HTML
            markupCount = text . show . length

            markupCount' :: [a] -> HTML
            markupCount' xs = block "span" [attr "class" $ "badge " ++ label] $ text (show len)
                 where len = length xs
                       label = if len == 0 then "badge-success" else "badge-important"

        let bad_links = map findBadLinks links

            br = block "br" [] mempty

        let page_tabel = block "table" [] $ mconcat $
                        [ block "tr" [] $ mconcat
                          [ block "th" [] $ text $ "#"
                          , block "th" [] $ text $ "Page Name"
                          , block "th" [attr "style" "text-align: center"] $ mconcat [text "file",br,text "type"]
                          , block "th" [attr "style" "text-align: right"] $ mconcat [text "local",br,text "links"]
                          , block "th" [attr "style" "text-align: right"] $ mconcat [text "extern",br,text "links"]
                          , block "th" [attr "style" "text-align: right"] $ mconcat [text "local",br,text "fail"]
                          , block "th" [attr "style" "text-align: right"] $ mconcat [text "extern",br,text "fail"]
                          , block "th" [attr "style" "text-align: center"] $ mconcat [text "bad links"]
                          ]
                        ] ++
                        [ block "tr" [] $ mconcat
                          [ block "td" [attr "style" "text-align: right"] $ text $ show n
                          , block "td" []
                            $ block "a" [attr "href" (ld_pageName page) ]
                              $ text $ ld_pageName page
                          , block "td" []
                            $ case lookup (html_prefixed $ ld_pageName page) inp of
                                 Nothing  -> text "ERROR"
                                 Just FromContent   -> text "content"
                                 Just AutoGenerated -> text "autogen"
                                 Just Redirect      -> text "redirect"
                                 Just Copy          -> text "copied"
                          , block "td" [attr "style" "text-align: right"] $ ld_localURLs page
                          , block "td" [attr "style" "text-align: right"] $ ld_remoteURLs page
                          , block "td" [attr "style" "text-align: right"] $ ld_localURLs page_bad
                          , block "td" [attr "style" "text-align: right"] $ ld_remoteURLs page_bad
                          , block "td" [] $ mconcat
                                [ text bad <> br
                                | bad <- ld_localURLs page_bad' ++ ld_remoteURLs page_bad'
                                ]

                          ]
                        | (n,page,page_bad,page_bad') <- zip4 [1..]
                                                    (map (fmap markupCount) links)
                                                    (map (fmap markupCount') bad_links)
                                                    (bad_links)
                        ]

        let colorURLCode :: URLResponse -> HTML
            colorURLCode (URLResponse [] _) =
                    block "span" [attr "class" $ "badge badge-important"]
                    $ block "i" [attr "class" "icon-warning-sign icon-white"]
                      $ text "" -- intentionally

            colorURLCode resp@(URLResponse xs _) =
                    mconcat $ [ block "span" [attr "class" $ "badge " ++ label] $ text $ show x
                              | x <- xs
                              ]
                where label = if goodLinkCode resp
                              then "badge-success"
                              else "badge-important"

        let timing (_,URLResponse _ t1) (_,URLResponse _ t2) = t1 `compare` t2

        let link_tabel = block "table" [] $ mconcat $
                        [ block "tr" [] $ mconcat
                          [ block "th" [] $ text $ "#"
                          , block "th" [] $ text $ "External URL"
                          , block "th" [attr "style" "text-align: center"] $ text $ "URL<BR/>code(s)"
                          , block "th" [attr "style" "text-align: right"] $ text $ "time<BR/>(ms)"
                          ]
                        ] ++
                        [ block "tr" [] $ mconcat
                          [ block "td" [attr "style" "text-align: right"] $ text $ show n
                          , block "td" []
                            $ block "a" [attr "href" url ]
                              $ text $ url
                          , block "td" [attr "style" "text-align: right"]
                            $ colorURLCode resp
                          , block "td" [attr "style" "text-align: right"] $ text $ show tm
                          ]
                        | (n,(url,resp@(URLResponse _ tm))) <- zip [1..] $ sortBy timing external_links
                        ]

        let f = block "div" [attr "class" "row"] . block "div" [attr "class" "span10  offset1"]

        return $ f $ mconcat
                [ block "h2" [] $ text "Status"
                , text $ "Nominal"
                , block "h2" [] $ text "Pages"
                , page_tabel
                , block "h2" [] $ text "External URLs"
                , link_tabel
                ]

findURL :: (Monad m) => Translate Context m Node String
findURL = promoteT $ do
                (nm,val) <- attrT (,)
                cxt@(Context (c:_)) <- contextT
                tag <- KURE.apply getTag cxt c
                case (nm,[tag]) of
                   ("href","a":_)     -> return val
                   ("href","link":_)  -> return val
                   ("src","script":_) -> return val
                   ("src","img":_)    -> return val
                   _                  -> fail "no correct context"


findLinks :: String -> Action (LinkData [String])
findLinks nm = do
        let name = dropDirectory1 (dropDirectory1 nm)

        txt <- readFile' nm
        let tree = parseHTML nm txt

        urls <- applyFPGM' (extractT $ collectT findURL) tree

--        liftIO $ print urls

        -- What about ftp?
        let isRemote url = ("http://" `isPrefixOf` url)
                        || ("https://" `isPrefixOf` url)

        let locals = [ takeDirectory name </> url
                     | url <- urls
                     , not (isRemote url)
                     ]

        let globals = filter isRemote urls

        return $ LinkData name locals globals
