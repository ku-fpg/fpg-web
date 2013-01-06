module BibTeX where


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

import Text.HTML.KURE

import Shake
