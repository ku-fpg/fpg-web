{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, LambdaCase, InstanceSigs #-}
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

import Text.XML.HXT.Parser.HtmlParsec
import Text.XML.HXT.DOM.ShowXml
import Text.XML.HXT.DOM.TypeDefs

import Language.KURE.Walker
import Data.Tree.NTree.TypeDefs


import qualified Language.KURE as KURE
import Language.KURE hiding (apply)


site_dir     = "site"
build_dir    = "_make"

all_src_files = build_dir </> "all_src_files" -- DEAD

main = do
        args <- getArgs
        main2 ["clean"]
        main2 args




main2 ["build"] = shake shakeOptions { shakeVerbosity = Diagnostic } $ do

--        want ["_make/html/index.html"]

        -- Require the final html files in place
        action $ do
                liftIO $ createDirectoryIfMissing True $ build_dir
                files <- getDirectoryFiles site_dir "//*.markdown"
                let target_files = map (flip replaceExtension ".html") files
                liftIO $ print target_files
                need (map (build_dir </> "html" </>) target_files)

--              -- To consider, save these for use in other contexts (like scp?)
--                writeFileLines all_src_files target_files

        -- make the indiviual html files for uploading, in the html directory.
        "_make/html//*.html" *> \ out -> do
                let srcName = build_dir </> "contents" </> dropDirectory1 (dropDirectory1 out)
                let tplName = "template" </> "page.html"
                need [ srcName , tplName ]
                liftIO $ print ("srcName",srcName)
                src <- readFile' srcName
                liftIO $ print src
                writeFile' out src

        -- make the content files, using pandoc.
        "_make/contents//*.html" *> \ out -> do
                let srcName = dropDirectory1 $ dropDirectory1 $ replaceExtension out ".markdown"
                let input = site_dir </> srcName
                liftIO $ print ("A",input)
                need [ input ]
                system' "pandoc" ["-o",out,input]


main2 ["clean"] = do
        b <- doesDirectoryExist build_dir
        when b $ do
           removeDirectoryRecursive build_dir
        return ()

main2 ["import"] = shake shakeOptions { shakeVerbosity = Diagnostic } $ do


        action $ do
                liftIO $ createDirectoryIfMissing True $ build_dir
        -- What you need to import
        want ["import/Research.markdown"]


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
                let res0 = fromKureM error $ KURE.apply (collectT $ q) () (XTrees contents)
                liftIO $ print ("len",length res0)
                let (XTree res:_) = res0
                liftIO $ putStrLn $ (take 50 $ show res0)
                let tmp_file = build_dir </> "tmp.html"
                liftIO $ writeFile tmp_file (xshow [res])
                liftIO $ print 3
                system' "pandoc" ["-f","HTML","-t","markdown","-o",out,tmp_file]


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

matchXTag :: Monad m => QName -> Rewrite c m (NTree XNode)
matchXTag tag = acceptR $ \ e -> case e of
        (NTree (XTag tag' _) _) -> tag == tag'
        _ -> False

--test = collectT (contextfreeT $ \ (

