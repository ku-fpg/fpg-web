module Web.Chione
        ( Build(..)
          -- * key directory names
         , build_dir
         , html_dir
         , admin_dir
         -- * Build target detection
         , findBuildTargets
         -- * KURE rewrites
         , findURL
         , templateToHTML
         , injectHTML
--        , module Web.Chione     -- include everything right now
        ) where

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

import Shake
import Text.HTML.KURE

-- | 'Build' is the various ways of building a final HTML webpage.

data Build = FromContent        -- ^ from content directory
           | AutoGenerated      -- ^ HTML generated somehow (sitemap, reference pages, etc)
           | Redirect           -- ^ simple redirect (need to look up table to avoid rebuilding)
           | Copy               -- ^ copy from root of repo (gif, css, etc)
           deriving (Show, Read, Eq, Ord)

-- | Name of location for all generated files.
-- Can always be removed safely, and rebuilt.
build_dir :: String
build_dir    = "_make"

-- | Name of location of our target HTML directory.
html_dir :: String
html_dir    = build_dir </> "html"

-- | Name of location of our admin HTML directory.
admin_dir :: String
admin_dir    = build_dir </> "admin"

-- | 'findBuildTargets' looks to find the names and build instructions for
-- the final website. It looks in the directories "site" for markdown files,
-- and "img", "js" and "css" for image, js and css files.
findBuildTargets :: IO [(String,Build)]
findBuildTargets = do
        site <- getRecursiveContents "site"
        img  <- getRecursiveContents "img"
        js   <-  getRecursiveContents "js"
        css  <- getRecursiveContents "css"
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
                 ]
  where
        html_prefixed = (html_dir </>)


-- (local to this module.)
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


------------------------------------------------------------------------------------

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


templateToHTML :: String -> R Node -> String -> Action ()
templateToHTML tplName tr outFile = do
        template <- readFile' tplName
        let page0 = parseHTML tplName template
        page1 <- applyFPGM' (extractR tr) page0
        writeFile' outFile $ show page1

-- | Replace given id (2nd argument) with an HTML file (filename is first argument).
--
-- > let tr = inject "Foo.hs" "contents"
--
injectHTML :: String -> String -> R Node
injectHTML fileName idName = prunetdR (promoteR (anyBlockHTML fn))
  where
        fn :: T Block HTML
        fn = do nm <- getAttr "id"
                debugR $ show ("inject",idName,nm)
                if nm == idName
                then translate $ \ _ _ -> do
                        file <- liftActionFPGM $ readFile' fileName
                        return $ parseHTML fileName file
                        -- read the file
                else fail "no match"


{--- T HTML [HTML]
unconcatHTML :: T HTML [HTML]
unconcatHTML = undefined

allRList :: R a -> R [a]
allRList
-}



-- T [HTML] HTML   -- easy, mconcat

debugR :: (Monad m, Show a) => String -> Rewrite c m a
debugR msg = acceptR (\ a -> trace (msg ++ " : " ++ take 100 (show a)) True)

-- template :: [(String,HTML)] -> T HTML HTML