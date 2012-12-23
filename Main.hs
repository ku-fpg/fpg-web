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

import Text.XML.HXT.Parser.HtmlParsec
import Text.XML.HXT.DOM.ShowXml
import Text.XML.HXT.DOM.TypeDefs

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




main2 ["build"] = shake shakeOptions { shakeVerbosity = Loud } $ do

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
                let local_prefix = concat (take (count - 2) (repeat "../"))

                liftIO $ print(out,count,local_prefix)

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

                    f ('/':rest) = replaceExtension (local_prefix ++ rest) "html"
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
                        | "../" `isPrefixOf` nm = dropDirectory1 nm
                        | otherwise             = nm
                let relativeURL ('/':rest) = replaceExtension (local_prefix ++ rest) "html"
                    relativeURL other      | "http://" `isPrefixOf` other
                                          || "https://" `isPrefixOf` other = other
                                           | otherwise = other

                -- The <i> icons can not use the <i/> versions
                let iconHack = promoteR $ do
                          (NTree t@(XTag tag _) []) <- idR
                          if tag == mkName "i"
                             then return (NTree t [NTree (XText "") []])
                             else idR

                let macro :: String -> Maybe [NTree XNode]
                    macro "fpg-contents" = return contents
                    macro _              = fail "macro failure"

                let macroExpand :: Translate XContext KureM (NTree XNode) [NTree XNode]
                    macroExpand = do
--                         tree@(NTree t []) <- idR
--                         () <- trace ("trace: " ++ show t) $ return ()
                         tree@(NTree t@(XTag tag _) _) <- idR
                         True <- return (tag == mkName "div")
--                         () <- trace ("trace: " ++ show tag) $ return ()
                         if tag == mkName "div"
                            then do clss <- extractT $ childT 0 $ crushbuT $ (getAttr >>> arr (: []))
                                    () <- trace ("trace: " ++ show clss) $ return ()
                                    case lookup (mkName "class") clss of
                                      Nothing -> fail "no macro match for div"
                                      Just nm -> case macro nm of
                                                   Nothing -> fail $ "no macro match for div / " ++ nm
                                                   Just t  -> pure t
                            else fail "no macro match"
                        -- * Needs to be a div
                        -- * Needs tag

                let fixTable :: Rewrite XContext KureM XTree
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
                let XTrees page0 = fromKureM error $ KURE.apply tpl_prog (noContext) (XTrees page)

                -- Now, we find the teaser links

                let teaser_links :: Translate XContext KureM XTree [String]
                    teaser_links = promoteT $ do
                          (NTree (XTag tag attrs) rest) <- idR
                          if tag == mkName "a"
                             then do clss <- extractT $ childT 0 $ crushbuT $ (getAttr >>> arr (: []))
--                                     () <- trace ("traceX: " ++ show clss) $ return ()
                                     case lookup (mkName "class") clss of
                                        Just "teaser" -> case lookup (mkName "href") clss of
                                                           Just url -> do
                                                                () <- trace ("traceX: " ++ show clss) $ return ()
                                                                pure [url]
                                                           _ -> fail "no href in teaser"
                                        _ -> fail "no teaser class in anchor"
                             else fail "no match for anchor"

                let urls = fromKureM error $ KURE.apply (crushbuT teaser_links) (noContext) (XTrees page0)

                liftIO $ print ("urls",urls, takeDirectory out)

                -- Make sure you already have the teaser contents. Assumes no loops.
                need [ takeDirectory out </> url | url <- urls ]

                let extract_teaser :: Translate XContext KureM XTree [NTree XNode]
                    extract_teaser = promoteT $ do
                          (NTree (XTag tag attrs) rest) <- idR
                          if tag == mkName "div"
                             then do clss <- extractT $ childT 0 $ crushbuT $ (getAttr >>> arr (: []))
                                     () <- trace ("traceX: " ++ show clss) $ return ()
                                     case lookup (mkName "class") clss of
                                        Just "teaser" -> pure rest
                                        _ -> fail "no teaser class in div"
                             else fail "no match for div"

                teaser_map <- sequence
                        [ do src <- readFile' (takeDirectory out </> url)
                             let remote_page = parseHtmlDocument (takeDirectory out </> url) src
                             let content0 = fromKureM error $ KURE.apply (onetdT extract_teaser) (noContext) (XTrees remote_page)
                             return (url,content0)
                        | url <- urls
                        ]



                liftIO $ print ("teaser_map",teaser_map)

                let insert_teaser :: Translate XContext KureM (NTree XNode) [NTree XNode]
                    insert_teaser = do
--                         tree@(NTree t []) <- idR
--                         () <- trace ("trace: " ++ show t) $ return ()
                          tree@(NTree t@(XTag tag _) rest) <- idR
--                          True <- return (tag == mkName "div")
--                          () <- trace ("traceZ: " ++ show tag) $ return ()
                          if tag == mkName "a"
                             then do clss <- extractT $ childT 0 $ crushbuT $ (getAttr >>> arr (: []))
                                     () <- trace ("trace!: " ++ show clss) $ return ()
                                     case lookup (mkName "class") clss of
                                        Just "teaser" ->
                                          case lookup (mkName "href") clss of
                                              Just url ->
                                                case lookup url teaser_map of
                                                   Just trees -> do
                                                        () <- trace ("traceY: " ++ show trees) $ return ()
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
                let XTrees page1 = fromKureM error $ KURE.apply teaser (noContext) (XTrees page0)

                writeFile' out $ xshow page1


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
                let q :: Translate () KureM (NTree XNode) [String]
                    q = matchXTag (mkName "a") >>> (tagT p idR $ \ _ a b -> [a])

                    p :: Translate () KureM [NTree XNode] String
                    p = extractT (oneT (promoteT r) :: Translate () KureM XTree String)

                    r :: Translate () KureM (NTree XNode) String
                    r = matchXAttr (mkName "href") >>> xattrT s (\ attr sub -> sub)

                    s :: Translate () KureM [NTree XNode] String
                    s = extractT (allT (promoteT t) :: Translate () KureM XTree String)

                    t :: Translate () KureM (NTree XNode) String
                    t = textT idR $ \ txt _ -> txt

                    {-
                    (extractT (oneT (promoteT s) :: Translate () KureM XTree String))

                    s :: Translate () KureM [NTree XNode] String
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

treeT :: (Monad m) => Translate XContext m XNode a -> Translate XContext m [NTree XNode] b -> (a -> b -> x) -> Translate XContext m (NTree XNode) x
treeT ta tb f = translate $ \ (XContext cs) (NTree node rest) ->
                let c = XContext (node : cs) in liftM2 f (KURE.apply ta c node) (KURE.apply tb c rest)


--test = collectT (contextfreeT $ \ (
genericT :: forall a b c
         .  (Injection c XTree)
         => (Translate () KureM XTree a -> Translate () KureM XTree b)
         -> Translate () KureM c a -> Translate () KureM c b
genericT f r = extractT (f (promoteT r))

--traceR :: (a -> String) -> Rewrite c m a
--traceR f = rewrite $ \ _ a -> trace (f a) a

--------------------------------

debugR :: (Show a) => Rewrite XContext KureM a
debugR = acceptR (\ a -> trace ("traceR: " ++ take 100 (show a)) True)

-- change an embedded URL
mapURL :: (String -> String) -> Rewrite XContext KureM XTree
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

getAttr :: Translate XContext KureM XTree (QName,String)
getAttr = promoteT $ do
                (NTree (XText txt) []) <- idR
                c <- contextT
                case c of
                    XContext (XAttr attr:_) -> return (attr,txt)
                    _ -> fail "getAttrib failed"


concatMapRXtree :: Translate XContext KureM (NTree XNode) [NTree XNode] -> Rewrite XContext KureM XTree
concatMapRXtree rr = promoteR $ rewrite $ \ c xs -> do
        xs' <- mapM (KURE.apply (extractT rr) c) xs
        return (concat xs' :: [NTree XNode])

--fillContent :: Translate KureM XTree (NTree XNode) [NTree XNode]
