{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, LambdaCase, InstanceSigs, FlexibleContexts #-}
-- @
--
--      site/ ... name.page
--
--      _make/contents/name.markdown
-- @

import Development.Shake hiding (getDirectoryContents)
import Development.Shake.FilePath

import System.Directory
import System.Environment
import Control.Monad
import qualified Control.Exception as E
import System.Posix (getSymbolicLinkStatus, isDirectory)
import Control.Arrow
import Control.Applicative hiding ((*>))
import Data.List
import Data.Char

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
--                liftIO $ removeFile "_make/html/index.html"
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
                liftIO $ print ("srcName",srcName)
                src <- readFile' srcName
--                liftIO $ print src

                -- now follow links
                let q :: Rewrite () KureM XTree
                    q = promoteR (matchXTag (mkName "a")) >>> prunetdR p

                    p :: Rewrite () KureM XTree
                    p = promoteR (matchXAttr (mkName "href")) >>> prunetdR r

                    r :: Rewrite () KureM XTree
                    r = promoteR (textT idR $ \ txt stuff -> NTree (XText $ f txt) stuff)

                    f ('/':rest) = replaceExtension (local_prefix ++ rest) "html"
                    f other      | "http://" `isPrefixOf` other
                                || "https://" `isPrefixOf` other = other
                                 | otherwise = "##bad URL " ++ other
{-

                    p :: Translate () KureM [NTree XNode] String
                    p = extractT (oneT (promoteT r) :: Translate () KureM XTree String)

                    r :: Translate () KureM (NTree XNode) String
                    r = matchXAttr (mkName "href") >>> xattrT s (\ attr sub -> sub)

                    s :: Translate () KureM [NTree XNode] String
                    s = extractT (allT (promoteT t) :: Translate () KureM XTree String)

                    t :: Translate () KureM (NTree XNode) String
                    t = textT idR $ \ txt _ -> txt
-}
                    {-
                    (extractT (oneT (promoteT s) :: Translate () KureM XTree String))

                    s :: Translate () KureM [NTree XNode] String
                    s = pure "X" -- textT idR $ \ txt _ -> txt
                    -}

                let contents = parseHtmlDocument "input" src
                let XTrees res0 = fromKureM error $ KURE.apply (tryR (prunetdR q)) () (XTrees contents)

                let src' = xshow res0

--                liftIO $ print ("res0",res0)

                writeFile' out src'

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
instance Walker () XTree where
        allR :: forall m . MonadCatch m => Rewrite () m XTree -> Rewrite () m XTree
        allR rr = prefixFailMsg "allR failed: " $
          rewrite $ \ c -> \ case
            XTree  ntree  -> liftM inject $ KURE.apply allRXtree c ntree
            XTrees ntrees -> liftM inject $ KURE.apply allRXtrees c ntrees
          where
          -- uses translations over XTrees
            allRXtree :: MonadCatch m => Rewrite () m (NTree XNode)
            allRXtree = rewrite $ \ c -> \ case
                            NTree x xs -> do
                                x' <- case x of
                                        XPi n ys -> liftM (XPi n) $ KURE.apply (extractR rr) c ys
                                        XTag n ys -> liftM (XTag n) $ KURE.apply (extractR rr) c ys
                                        other -> return other
                                xs' <- KURE.apply (extractR rr) c xs
                                return $ NTree x' xs'

          -- uses translations over XTree
            allRXtrees :: MonadCatch m => Rewrite () m [NTree XNode]
            allRXtrees = rewrite $ \ c xs -> mapM (KURE.apply (extractR rr) c) xs

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


--test = collectT (contextfreeT $ \ (
genericT :: forall a b c
         .  (Injection c XTree)
         => (Translate () KureM XTree a -> Translate () KureM XTree b)
         -> Translate () KureM c a -> Translate () KureM c b
genericT f r = extractT (f (promoteT r))


