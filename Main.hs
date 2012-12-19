import Development.Shake hiding (getDirectoryContents)
import Development.Shake.FilePath

import System.Directory
import System.Environment
import Control.Monad
import qualified Control.Exception as E
import System.Posix (getSymbolicLinkStatus, isDirectory)


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

