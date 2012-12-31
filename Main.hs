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
        main2 ["clean"]
        main2 args


data Build = FromContent        -- from content directory
           | AutoGenerated      -- HTML generated somehow (sitemap, reference pages, etc)
           | Redirect           -- simple redirect (need to look up table to avoid rebuilding)
           | Copy               -- copy from root of repo (gif, css, etc)
           | Paper              -- generate a page about a paper
           deriving (Show, Read, Eq, Ord)

main2 ["build"] = do

--    build_cmds <- findBuildDirections

    meta_contents :: ShakeVar [(String,Build)]  <- newShakeVar "_make/meta/contents.txt"
    meta_redirect :: ShakeVar [(String,String)] <- newShakeVar "_make/meta/redirect.txt"
    meta_bibtex   :: ShakeVar [(String,BibTeX)] <- newShakeVar "_make/meta/bibtex.txt"

    shake shakeOptions { shakeVerbosity = Loud
--                                     , shakeReport = return "report.html"
--                                     , shakeThreads = 4
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
                liftIO $ print ("A",input)
                need [ input ]
                system' "pandoc" ["-o",out,input]

        "_make/autogen/Sitemap.html" *> \ out -> do
                contents :: [(String,Build)] <- readShakeVar meta_contents
                let trees = genSiteMap "/" (map (("/" ++) . dropDirectory1 . dropDirectory1 . fst) contents)
                let sitemap = mkElement (mkName "p") [] trees
                writeFileChanged out $ (xshow [sitemap])

        "_make/autogen/Papers/*.html" *> \ out -> do
                liftIO $ print "######################"
                need [ "data/fpg.bib" ]
                bib <- readShakeVar meta_bibtex
                let bib' =  [ (tagToFileName nm,(nm,dat))
                            | (nm,dat) <- bib
                            ]
                case lookup (takeFileName out) bib' of
                  Nothing -> error $ "abort: " ++ show out
                  Just (nm,bb@(BibTeX _ stuff)) -> do
                    writeFile' out
                        "PAPERS.HTML file"
{-
                        $ xshow
                        [ mkElement (mkName "div")
                                [ mkAttr (mkName "class") [mkText "row"]]
                                [ mkElement (mkName "div")
                                        [ mkAttr (mkName "class") [mkText "span8 offset2"]]
                                        [ mkElement (mkName "p")
                                                []
                                                (buildBibCite nm bb)
                                        , mkElement (mkName "h3") [] [mkText "Abstract"]
                                        , mkElement (mkName "blockquote")
                                                []
                                                [ case lookup "abstract" stuff of
                                                    Just txt -> mkText $ txt
                                                    Nothing -> mkText $ "no abstract"
                                                ]
                                        , mkElement (mkName "h3") [] [mkText "BibTeX"]
                                        , mkElement (mkName "pre")
                                                [ mkAttr (mkName "style") [mkText "font-size: 70%"]]
                                                [ asciiBibNode nm bb ]
                                         ]
                                 ]
                        ]
-}
        "_make/autogen/Publications.html" *> \ out -> do
                liftIO $ print "######################"
                need [ "data/fpg.bib" ]
                bib <- readShakeVar meta_bibtex
                let years :: [Int] = reverse $ sort $ nub
                                [ read y
                                | (_,BibTeX _ stuff) <- bib
                                , Just y <- [lookup "year" stuff]
                                ]
{-


                let entries = concat
                        [ [  mkElement (mkName "div")
                                [ mkAttr (mkName "class") [mkText "row"]]
                                [ mkElement (mkName "div")
                                        [ mkAttr (mkName "class") [mkText "span1 offset1"]]
                                        [ mkElement (mkName "h3")
                                                [mkAttr (mkName "style") [mkText "margin-top: -7px; border-top: 1px dotted;"]]
                                                [mkText $ show year ]]
                                , mkElement (mkName "div")
                                        [ mkAttr (mkName "class") [mkText "span8"]]
                                        [ mkElement (mkName "ul")
                                                []
                                                [ mkElement (mkName "li")
                                                   [mkAttr (mkName "style") [mkText "margin-bottom: 2px;"]]
                                                   (buildBibCite nm dat)
                                                | (nm,dat@(BibTeX _ stuff)) <- bib
                                                , Just y <- [lookup "year" stuff]
                                                , read y == year
                                                ]
                                        ]
                                ]
                          ]
                        | year <- years
                        ]
-}
                liftIO $ print ("years",years)
                writeFile' out
                        $ xshow
                        $ [] -- entries

        -- Finally, building visable results, all in the html sub-directory
        ("_make/html//*") *> \ out -> do
                build_cmds <- readShakeVar meta_contents
                case lookup out build_cmds of
                  Just Copy        -> do
                        let src = dropDirectory1 $ dropDirectory1 $ out
                        need [ src ]
                        system' "cp" [src,out]
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
                        ["_make/html/index.html"]
--                        []
--                b <- doesFileExist "_make/autogen/Papers/Framer_12_HERMITinMachine.html"
                sequence_ [ do
                        b <- doesFileExist f
                        liftIO $ if b then removeFile f
                                 else return () | f <- work ]

                build_cmds <- readShakeVar meta_contents
                need $ map fst $ build_cmds


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
        return $ [ (prefixed $ dropDirectory1 $ flip replaceExtension "html" $ file,FromContent)
                 | file <- site
                 , "site//*.markdown" ?== file
                 ] ++
                 [ (prefixed file,Copy)
                 | file <- img ++ js ++ css
                 , "img//*.gif" ?== file
                        || "img//*.png" ?== file
                        || "js//*.js" ?== file
                        || "css//*.css" ?== file
                 ] ++
                 [ (prefixed file,Redirect)
                 | file <- map fst redirects
                 ] ++
                 [ (prefixed file,AutoGenerated)
                 | file <- ["Sitemap.html","Publications.html"]
                 ] ++
                 [ (prefixed (paper_page_dir </> tagToFileName tag),AutoGenerated)
                 | (tag,_) <- bib
                 ]

  where prefixed = (build_dir </> "html" </>)

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
newtype FPGM a = FPGM { runFPGM :: Action (Either String a) }

--                let urls = fromKureM error $ KURE.apply (crushbuT teaser_links) (noContext) (XTrees page0)

applyFPGM' :: Translate Context FPGM a b -> a -> Action b
applyFPGM' t a = do
        res <- runFPGM $ KURE.apply t (Context []) a
        case res of
          Left msg -> error $ "applyFPGM " ++ msg
          Right a -> return a

liftActionFPGM :: Action a -> FPGM a
liftActionFPGM = FPGM . fmap Right

type T a b = Translate Context FPGM a b
type R a   = T a a

instance Monad FPGM where
        return a = FPGM (return (Right a))
        m1 >>= k = FPGM $ do
                r <- runFPGM m1
                case r of
                  Left msg -> return (Left msg)
                  Right a -> runFPGM (k a)
        fail = FPGM . return . Left

instance Functor FPGM where
        fmap f m = pure f <*> m

instance Applicative FPGM where
        pure a = return a
        af <*> aa = af >>= \ f -> aa >>= \ a -> return (f a)

instance MonadCatch FPGM where
        catchM m1 handle = FPGM $ do
                r <- runFPGM m1
                case r of
                  Left msg -> runFPGM $ handle msg
                  Right a -> return (Right a)

instance MonadIO FPGM where
        liftIO m = FPGM (Right <$> liftIO m)

-----------------------------------------------------------

debugR :: (Monad m, Show a) => String -> Rewrite c m a
debugR msg = acceptR (\ a -> trace (msg ++ " : " ++ take 100 (show a)) True)

-- change an embedded URL

mapURL' :: (Monad m) => (String -> String) -> Rewrite Context m Node
mapURL' f = promoteR $ do
                (nm,val) <- attrT (,)
                Context c <- contextT
--                () <- trace (show ("mapURL",nm,val,c)) $ return ()
                case (nm,c) of
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

                liftIO $ print(out,count,local_prefix "")


                -- Compute breadcrumbs
                let path0 = [ nm | nm <- splitDirectories $ dropDirectory1 $ dropDirectory1 $  out ]

                -- index itself does not have an entry, but *is* home.
                let path1 = case path0 of
                             ["index.html"] -> []
                             _               -> path0

                need [ srcName , tplName ]

                -- next, the contents
                liftIO $ print ("srcName",srcName)
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
                            () <- trace (show ("tm",tm,txt)) $ return ()
                            return (text txt)
                    macro "fpg-sitemap" = do
                            liftIO $ print "FPG-SITEMAP"
                            liftActionFPGM $ do
                                let fileName = build_dir </> "autogen" </> "Sitemap.html"
                                need [ fileName ]
                                txt <- readFile' fileName
                                liftIO $ print $ "FPG: " ++ txt
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
                                  return $ attrs (attr "class" "table table-bordered table-condensed" : ss)

                let fixLandingPage :: Rewrite Context FPGM Node
                    fixLandingPage = promoteR $ do
                          guardMsg (path0 == ["index.html"]) "is not landing page"
                          "body" <- getTag
                          extractR' $ anyR $ promoteR' $ do
                                  ss <- attrsT idR id
                                  return $ attrs (attr "class" "fpg-landing" : ss)

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

                            () <- trace (show ("url",inside_content)) $ return ()

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
                                  return $ attrs (attr "class" "active" : ss)


                let tpl_prog :: Rewrite Context FPGM Node
                    tpl_prog = tryR (prunetdR (mapURL' normalizeTplURL))
                           >>> tryR (alltdR (tryR (promoteR (mapHTML (macroExpand <+ arr html)))))
                           >>> tryR (alltdR (tryR (promoteR (mapHTML (insertTeaser <+ arr html)))))
                           >>> tryR (prunetdR (mapURL' relativeURL))
                           >>> tryR (alltdR (tryR (promoteR findActive)))
                           >>> tryR (prunetdR fixTable)
                           >>> tryR (prunetdR fixLandingPage)

                page1 <- applyFPGM' (extractR tpl_prog) page0

                writeFile' out $ show page1

                liftIO $ print "DONE"

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


asciiBibNode :: String -> BibTeX -> NTree XNode -- escaped
asciiBibNode id (BibTeX ty stuff) = mkText $ unlines $
        ["@" ++ ty ++ "{" ++ id ++ ","] ++
        [ "    " ++ tag ++ " = {" ++ dat ++ "}"
        | (tag,dat) <- stuff
        , tag /= "abstract"
        , head tag /= 'X'
        ] ++
        ["}"]

-- Build textual citatation, with link(s).
buildBibCite :: String -> BibTeX -> [NTree XNode]
buildBibCite id (BibTeX ty stuff) =
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

--          E.Cons {E.entryType :: String,
--            E.identifier :: String,
--            E.fields :: [(String, String)]}

--buildBibPage ::

tagToFileName :: String -> String
tagToFileName nm = map fn nm ++ ".html"
  where
          fn ':' = '_'
          fn o   = o