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


site_dir     = "site"
build_dir    = "_make"

all_src_files = build_dir </> "all_src_files" -- DEAD

main = do
        args <- getArgs
        main2 ["clean"]
        main2 args


main2 ["build"] = shake shakeOptions { shakeVerbosity = Loud
--                                     , shakeReport = return "report.html"
--                                     , shakeThreads = 4
                                     } $ do

--        want ["_make/html/index.html"]

        -- Require the final html files in place
        action $ do
                b <- doesFileExist "_make/html/index.html"
                liftIO $ if b then removeFile "_make/html/index.html"
                              else return ()
                liftIO $ createDirectoryIfMissing True $ build_dir      -- is this needed?

                html_files <- ( map dropDirectory1
                              . map (flip replaceExtension ".html")
                              . filter ("site//*.markdown" ?==))
                                <$> (liftIO $ getRecursiveContents "site")
                img_files <- filter (\ nm -> "img//*.gif" ?== nm
                                          || "img//*.png" ?== nm
                                     )
                                <$> (liftIO $ getRecursiveContents "img")
                js_files <- filter ("js//*.js" ?==)
                                <$> (liftIO $ getRecursiveContents "js")
                css_files <- filter ("css//*.css" ?==)
                                <$> (liftIO $ getRecursiveContents "css")


                let target_files = map (build_dir </> "html" </>)
                                 $ html_files
                                        ++ img_files
                                        ++ js_files
                                        ++ css_files

                liftIO $ print target_files
                need target_files

--              -- To consider, save these for use in other contexts (like scp?)
--                writeFileLines all_src_files target_files

        -- make the indiviual html files for uploading, in the html directory.
        "_make/html//*.html" *> \ out -> do
--                liftIO $ prepareDirectory out
                let srcName = build_dir </> "contents" </> dropDirectory1 (dropDirectory1 out)
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


                let crumbs = [mkElement (mkName "li") []
                                [mkElement (mkName "a") [mkAttr (mkName "href") [mkText "ABCD"]]
                                   []
                                ]
                             ]

--                            <li><a href="#">Library</a> <span class="divider">/</span></li>
--    <li class="active">Data</li>


                liftIO $ print("path",path1,crumbs)

                need [ srcName , tplName ]

                -- next, the contents
                liftIO $ print ("srcName",srcName)
                src <- readFile' srcName
--                liftIO $ print src

                -- now follow links
                let q :: Rewrite XContext KureM XTree
                    q = promoteR (do
                          (NTree (XText txt) []) <- idR
                          c <- contextT
                          case c of
                            XContext (XAttr href:XTag a _:_)
                                | href == mkName "href" && a == mkName "a" -> do
                                    return (NTree (XText $ f txt) [])
                            _ -> fail "not correct context for txt")

                    f ('/':rest) = replaceExtension (local_prefix rest) "html"
                    f other      | "http://" `isPrefixOf` other
                                || "https://" `isPrefixOf` other = other
                                 | otherwise = "##bad URL " ++ other

                let contents = parseHtmlDocument "input" src
--                let XTrees contents0 = fromKureM error $ KURE.apply (tryR (prunetdR q)) (noContext) (XTrees contents)

--                let src' = xshow contents0
--                liftIO $ print ("res0",res0)

                -- next, read the template
                template <- readFile' tplName
                let page = parseHtmlDocument tplName template
                let normalizeTplURL nm
                        -- should really check for ccs, js, img, etc.
                        | "../" `isPrefixOf` nm = local_prefix (dropDirectory1 nm)
                        | otherwise             = nm
                let relativeURL ('/':rest) = replaceExtension (local_prefix rest) "html"
                    relativeURL other      | "http://" `isPrefixOf` other
                                          || "https://" `isPrefixOf` other = other
                                           | otherwise = other

                -- The <i> icons can not use the <i/> versions
                let iconHack = promoteR $ do
                          (NTree t@(XTag tag _) []) <- idR
                          if tag == mkName "i"
                             then return (NTree t [NTree (XText "") []])
                             else idR

                let macro :: String -> FPGM [NTree XNode]
                    macro "fpg-contents" = return contents
                    macro "fpg-update-time" = do
                            tm <- liftIO $ getZonedTime
                            let txt = formatTime defaultTimeLocale rfc822DateFormat tm
                            () <- trace (show ("tm",tm,txt)) $ return ()
                            return [mkText $ txt]
                    macro _              = fail "macro failure"

                let macroExpand :: Translate XContext FPGM (NTree XNode) [NTree XNode]
                    macroExpand = do
--                         tree@(NTree t []) <- idR
--                         () <- trace ("trace: " ++ show t) $ return ()
                         tree@(NTree t@(XTag tag _) _) <- idR
                         True <- return (tag == mkName "div" || tag == mkName "span")
--                         () <- trace ("trace: " ++ show tag) $ return ()
                         if True -- tag == mkName "div"
                            then do clss <- extractT $ childT 0 $ crushbuT $ (getAttr >>> arr (: []))
--                                    () <- trace ("trace: " ++ show clss) $ return ()
                                    case lookup (mkName "class") clss of
                                      Nothing -> fail "no macro match for div"
                                      Just nm -> constT $ macro nm
                            else fail "no macro match"
                        -- * Needs to be a div
                        -- * Needs tag

                let fixTable :: Rewrite XContext FPGM XTree
                    fixTable = promoteR $ do
                          (NTree (XTag tag attrs) rest) <- idR
                          if tag == mkName "table"
                             then return (NTree (XTag tag (attrs ++
                                     [NTree (XAttr $ mkName "class") [NTree (XText "table table-bordered table-condensed") []]])) rest)
                             else fail "not table"

                let tpl_prog = tryR (prunetdR (mapURL normalizeTplURL))
                           >>> tryR (prunetdR iconHack)
                           >>> alltdR (tryR (allT (promoteT (macroExpand <+ arr (: []))) >>> arr XTrees))
                           >>> tryR (prunetdR (mapURL relativeURL))
                           >>> tryR (prunetdR fixTable)
                XTrees page0 <- applyFPGM tpl_prog (XTrees page)

                -- Now, we find the teaser links

                let teaser_links :: Translate XContext FPGM XTree [String]
                    teaser_links = promoteT $ do
                          (NTree (XTag tag attrs) rest) <- idR
                          if tag == mkName "a"
                             then do clss <- extractT $ childT 0 $ crushbuT $ (getAttr >>> arr (: []))
--                                     () <- trace ("traceX: " ++ show clss) $ return ()
                                     case lookup (mkName "class") clss of
                                        Just "teaser" -> case lookup (mkName "href") clss of
                                                           Just url -> do
--                                                                () <- trace ("traceX: " ++ show clss) $ return ()
                                                                pure [url]
                                                           _ -> fail "no href in teaser"
                                        _ -> fail "no teaser class in anchor"
                             else fail "no match for anchor"

                urls <- applyFPGM (crushbuT teaser_links) (XTrees page0)

                liftIO $ print ("urls",urls, takeDirectory out)

                -- Make sure you already have the teaser contents. Assumes no loops.
                need [ takeDirectory out </> url | url <- urls ]

                let extract_teaser :: Translate XContext FPGM XTree [NTree XNode]
                    extract_teaser = promoteT $ do
                          (NTree (XTag tag attrs) rest) <- idR
                          if tag == mkName "div"
                             then do clss <- extractT $ childT 0 $ crushbuT $ (getAttr >>> arr (: []))
--                                     () <- trace ("traceX: " ++ show clss) $ return ()
                                     case lookup (mkName "class") clss of
                                        Just "teaser" -> pure rest
                                        _ -> fail "no teaser class in div"
                             else fail "no match for div"

                teaser_map <- sequence
                        [ do src <- readFile' (takeDirectory out </> url)
                             let remote_page = parseHtmlDocument (takeDirectory out </> url) src
                             content0 <- applyFPGM (onetdT extract_teaser) (XTrees remote_page)
                             return (url,content0)
                        | url <- urls
                        ]

                liftIO $ print ("teaser_map",teaser_map)

                let insert_teaser :: Translate XContext FPGM (NTree XNode) [NTree XNode]
                    insert_teaser = do
--                         tree@(NTree t []) <- idR
--                         () <- trace ("trace: " ++ show t) $ return ()
                          tree@(NTree t@(XTag tag _) rest) <- idR
--                          True <- return (tag == mkName "div")
--                          () <- trace ("traceZ: " ++ show tag) $ return ()
                          if tag == mkName "a"
                             then do clss <- extractT $ childT 0 $ crushbuT $ (getAttr >>> arr (: []))
--                                     () <- trace ("trace!: " ++ show clss) $ return ()
                                     case lookup (mkName "class") clss of
                                        Just "teaser" ->
                                          case lookup (mkName "href") clss of
                                              Just url ->
                                                case lookup url teaser_map of
                                                   Just trees -> do
--                                                        () <- trace ("traceY: " ++ show trees) $ return ()
                                                        -- Need to renormalize URLs
                                                        pure (trees ++ [NTree (XTag (mkName "a")
                                                                          [ NTree (XAttr (mkName "href")) [NTree (XText url) []]
                                                                          ])
                                                                          ([ NTree (XTag (mkName "i")
                                                                                [ NTree (XAttr (mkName "class"))
                                                                                        [NTree (XText "icon-chevron-right") []]
                                                                                ])
                                                                                [NTree (XText "") []]
                                                                           , NTree (XText " ") []
                                                                           ] ++ rest)
                                                                       ])

--                             <i class="icon-chevron-right"></i> <a href="About.html" class="">Read more about About Functional Programming at KU</a>

                                                   Nothing -> fail "can not find url teaser"
                                              _ -> fail "no href in teaser"
                                        _ -> fail "no teaser class in anchor"
                             else fail "no match for anchor"

                let teaser = alltdR (tryR (allT (promoteT (insert_teaser <+ arr (: []))) >>> arr XTrees))
                XTrees page1 <- applyFPGM teaser (XTrees page0)

--
--              Add the marker for links that are *this* page
--

--  <li class="active"><a href="#"><i class="icon-home"></i> Home</a></li>

                let findActive :: R XTree
                    findActive = promoteR $ do
                            matchTag "li"
                            -- Find the inner link
                            let findHR :: T XTree ()
                                findHR = promoteT $ do
                                    matchTag "a"
                                    attrs <- getAttrs
                                    case lookup (mkName "href") attrs of
                                      -- Fix for recusive names
                                      Just nm | nm == dropDirectory1 (dropDirectory1 out) -> return ()
                                      _ -> fail "findHR failed"
                            -- check to see if we contain our own link
                            extractT $ onetdT $ findHR
                            idR

                            -- grab the tree, add a class active
                            (NTree (XTag tag attrs) rest) <- idR
                            pure $ NTree (XTag tag (mkAttr (mkName "class") [mkText "active"] : attrs))
                                          rest

                XTrees page2 <- applyFPGM (tryR $ prunetdR findActive) (XTrees page1)

                writeFile' out $ xshow page2


        -- make the content files, using pandoc.
        "_make/contents//*.html" *> \ out -> do
--                liftIO $ prepareDirectory out
                let srcName = dropDirectory1 $ dropDirectory1 $ replaceExtension out ".markdown"
                let input = site_dir </> srcName
                liftIO $ print ("A",input)
                need [ input ]
                system' "pandoc" ["-o",out,input]

        [ "_make/html/img/*.gif",
          "_make/html/img/*.png",
          "_make/html/js/*.js",
          "_make/html/css/*.css"] **> \ out -> do
                system' "cp" [dropDirectory1 $ dropDirectory1 $ out,out]


main2 ["clean"] = do
        b <- doesDirectoryExist build_dir
        when b $ do
           removeDirectoryRecursive build_dir
        return ()

main2 ["import"] = do
        let start = ["import/Home.markdown"]
        v <- newMVar start
        shake shakeOptions { shakeVerbosity = Diagnostic } $ do


        action $ do
                liftIO $ createDirectoryIfMissing True $ build_dir
                liftIO $ createDirectoryIfMissing True $ "import"

        -- What you need to import
        want start   -- root
{-
        "import//*.markdown" *> \ out -> do
                let url = "http://ittc.ku.edu/csdl/fpg/" ++ dropDirectory1 (replaceExtension out "")
--                liftIO $ print (out,url)
                (std_out,std_err) <- systemOutput "curl" [url]
                let contents = parseHtmlDocument url std_out
--                liftIO $ putStrLn $ xshow contents
                let q = (acceptR $ \ e -> case e of
                           (XTree (NTree (XTag tag [NTree (XAttr cls) [NTree (XText "content") []]]) _)) ->
                                tag == mkName "div" && cls == mkName "class"
                           _ -> False)
                liftIO $ print 1
                let res0 = fromKureM error $ KURE.apply (collectPruneT $ q) () (XTrees contents)
                liftIO $ print ("len",length res0)
                let (XTree res:_) = res0
                liftIO $ putStrLn $ (take 50 $ show res0)
                let tmp_file = build_dir </> "tmp.html"
                liftIO $ writeFile tmp_file (xshow [res])
                liftIO $ print 3
                system' "pandoc" ["-f","HTML","-t","markdown","-o",out,tmp_file]

                -- now follow links
                let q :: Translate () FPGM (NTree XNode) [String]
                    q = matchXTag (mkName "a") >>> (tagT p idR $ \ _ a b -> [a])

                    p :: Translate () FPGM [NTree XNode] String
                    p = extractT (oneT (promoteT r) :: Translate () FPGM XTree String)

                    r :: Translate () FPGM (NTree XNode) String
                    r = matchXAttr (mkName "href") >>> xattrT s (\ attr sub -> sub)

                    s :: Translate () FPGM [NTree XNode] String
                    s = extractT (allT (promoteT t) :: Translate () FPGM XTree String)

                    t :: Translate () FPGM (NTree XNode) String
                    t = textT idR $ \ txt _ -> txt

                    {-
                    (extractT (oneT (promoteT s) :: Translate () FPGM XTree String))

                    s :: Translate () FPGM [NTree XNode] String
                    s = pure "X" -- textT idR $ \ txt _ -> txt
                    -}

                let res0 :: [String] = fromKureM error $ KURE.apply (prunetdT (promoteT q)) () (XTrees contents)

                let prefix = "/csdl/fpg/"
                let res1 = [ drop (length prefix) x |  x <- res0, prefix `isPrefixOf` x ]

                sequence_ [ liftIO $  createDirectoryIfMissing True $ "import" ++ takeDirectory1 x
                          | x <- res1
                          , '/' `elem` x
                          , isUpper (head x)
                          , not ("biblio" `isPrefixOf` x)
                          , not ("node" `isPrefixOf` x)
                          , not ("user" `isPrefixOf` x)
                          , not ("book" `isPrefixOf` x)
                          ]
--                 liftIO $ createDirectoryIfMissing True $ "import"

                let res2 = nub (filter (not . null) res1)
                let res3 = [ "import/" ++ x ++ ".markdown" | x <- res2
                               , x /= "front_page"
                          , not ("biblio" `isPrefixOf` x)
                          , not ("node" `isPrefixOf` x)
                          , not ("user" `isPrefixOf` x)
                          , not ("book" `isPrefixOf` x)
                          ]
                var <- liftIO $ takeMVar v
                liftIO $ print var
                liftIO $ putMVar v (out : var)
                let res4 = [ x | x <- res3, not (x `elem` (out:var)) ]
                liftIO $ print ("need",out,res4)
                need res4
                return ()
-}
main2 _ = putStrLn $ unlines
        [ "usage:"
        , "./Main clean          clean up"
        ]


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

applyFPGM :: T a b -> a -> Action b
applyFPGM t a = do
        res <- runFPGM $ KURE.apply t noContext a
        case res of
          Left msg -> error $ "applyFPGM " ++ msg
          Right a -> return a

type T a b = Translate XContext FPGM a b
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

newtype XContext = XContext [XNode]  -- list of all nodes on the way down
        deriving Show

noContext = XContext []

data XTree = XTree (NTree XNode)
           | XTrees [NTree XNode]
         deriving Show

instance Injection (NTree XNode) XTree where
        inject = XTree
        project (XTree t) = Just t
        project _         = Nothing

instance Injection [NTree XNode] XTree where
        inject = XTrees
        project (XTrees t) = Just t
        project _          = Nothing

-- Really simple!
instance Walker XContext XTree where
        allR :: forall m . MonadCatch m => Rewrite XContext m XTree -> Rewrite XContext m XTree
        allR rr = prefixFailMsg "allR failed: " $
          rewrite $ \ c -> \ case
            XTree  ntree  -> liftM inject $ KURE.apply (allRXtree rr) c ntree
            XTrees ntrees -> liftM inject $ KURE.apply (allRXtrees rr) c ntrees

-- uses translations over XTrees; treeT is used to fix context.
allRXtree :: MonadCatch m => Rewrite XContext m XTree -> Rewrite XContext m (NTree XNode)
allRXtree rr = treeT rrNode rrRest $ NTree
   where
           rrNode = rewrite $ \ c  -> \ case
                XPi  n ys -> liftM (XPi n)  $ KURE.apply (extractR rr) c ys
                XTag n ys -> liftM (XTag n) $ KURE.apply (extractR rr) c ys
                other     -> return other
           rrRest = extractR rr

-- uses translations over XTree
allRXtrees :: MonadCatch m => Rewrite XContext m XTree -> Rewrite XContext m [NTree XNode]
allRXtrees rr = rewrite $ \ c xs -> mapM (KURE.apply (extractR rr) c) xs

allRL :: (Monad m) => Translate () m a b -> Translate () m [a] [b]
allRL rr = translate $ \ c -> mapM (KURE.apply rr c)

-- Rewrite in terms of tagT
matchXTag :: Monad m => QName -> Rewrite c m (NTree XNode)
matchXTag tag = acceptR $ \ e -> case e of
        (NTree (XTag tag' _) _) -> tag == tag'
        _ -> False

matchXAttr :: Monad m => QName -> Rewrite c m (NTree XNode)
matchXAttr tag = acceptR $ \ e -> case e of
        (NTree (XAttr tag') _) -> tag == tag'
        _ -> False

matchXText :: Monad m => Rewrite c m (NTree XNode)
matchXText      = acceptR $ \ e -> case e of
        (NTree (XText _) _) -> True
        _ -> False

{-
tagT :: (Monad m) => Translate c m [NTree XNode] a -> Translate c m [NTree XNode] b -> (QName -> a -> b -> x) -> Translate c m (NTree XNode) x
tagT ta tb f = translate $ \ c -> \ case
         (NTree (XTag tag attrs) rest) -> liftM2 (f tag) (KURE.apply ta c attrs) (KURE.apply tb c rest)
         _ -> fail "not XTag"

xattrT :: (Monad m) => Translate c m [NTree XNode] a -> (QName -> a -> x) -> Translate c m (NTree XNode) x
xattrT ta f = translate $ \ c -> \ case
         (NTree (XAttr attr) rest) -> liftM (f attr) (KURE.apply ta c rest)
         _ -> fail "not XAttr"

textT :: (Monad m) => Translate c m [NTree XNode] a -> (String -> a -> x) -> Translate c m (NTree XNode) x
textT ta f = translate $ \ c -> \ case
         (NTree (XText text) rest) -> liftM (f text) (KURE.apply ta c rest)
         _ -> fail "not XText"
-}

treeT :: (Monad m) => Translate XContext m XNode a -> Translate XContext m [NTree XNode] b -> (a -> b -> x) -> Translate XContext m (NTree XNode) x
treeT ta tb f = translate $ \ (XContext cs) (NTree node rest) ->
                let c = XContext (node : cs) in liftM2 f (KURE.apply ta c node) (KURE.apply tb c rest)


--------------------------------

debugR :: (Show a) => String -> R a
debugR msg = acceptR (\ a -> trace (msg ++ " : " ++ take 100 (show a)) True)

-- change an embedded URL
mapURL :: (Monad m) => (String -> String) -> Rewrite XContext m XTree
mapURL f = promoteR $ do
                  (NTree (XText txt) []) <- idR
                  c <- contextT
                  case c of
                    XContext (XAttr href:XTag a _:_)
                        | href == mkName "href" && a == mkName "a" -> do
                            return (NTree (XText $ f txt) [])
                    XContext (XAttr href:XTag link _:_)
                        | href == mkName "href" && link == mkName "link" -> do
                            return (NTree (XText $ f txt) [])
                    XContext (XAttr src:XTag script _:_)
                        | src == mkName "src" && script == mkName "script" -> do
                            return (NTree (XText $ f txt) [])
                    XContext (XAttr src:XTag img _:_)
                        | src == mkName "src" && img == mkName "img" -> do
                            return (NTree (XText $ f txt) [])
                    _ -> fail "not correct context for txt"

getAttr :: Monad m => Translate XContext m XTree (QName,String)
getAttr = promoteT $ do
                (NTree (XText txt) []) <- idR
                c <- contextT
                case c of
                    XContext (XAttr attr:_) -> return (attr,txt)
                    _ -> fail "getAttrib failed"

getAttrs :: T (NTree XNode) [(QName,String)]
getAttrs = extractT $ childT 0 $ crushbuT $ find
  where
          find :: T XTree [(QName,String)]
          find = promoteT $ do
                  (NTree (XText txt) []) <- idR
                  XContext (XAttr attr:_) <- contextT
                  return [(attr,txt)]

matchTag :: String -> T (NTree XNode) ()
matchTag nm = do
        (NTree (XTag tag _) rest) <- idR
        if tag == mkName nm then return ()
                            else fail "matchTag failed"

{-
--getTree :: (QName -> Bool) -> Translate XContext m XTree (QName,[(QName,String)],NTrees)
extractTag :: Bool -> T (NTree XNode) ([(QName, String)], NTrees XNode)
extractTag pred = do
        (NTree (XTag tag _) rest) <- idR
        if mkName nm == tag then do clss <- extractT $ childT 0 $ crushbuT $ (getAttr >>> arr (: []))
                                    return (clss,rest)
                    else fail "getTree failed"
-}

--fillContent :: Translate FPGM XTree (NTree XNode) [NTree XNode]
