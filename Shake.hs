{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Shake.X where

import Development.Shake hiding (getDirectoryContents)
import Development.Shake.FilePath
import Control.Applicative hiding ((*>))
import Control.Monad.IO.Class
import qualified Language.KURE as KURE
import Language.KURE hiding (apply)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Monoid
import Text.HTML.KURE(Context)

data ShakeVar a = ShakeVar (TVar (Maybe a)) String (Action a)

newShakeVar :: String -> Action a -> IO (ShakeVar a)
newShakeVar nm act = do
        v <- atomically $ newTVar $ Nothing
        return $ ShakeVar v nm act

infix 1 !>

shakeVarRule :: (Show a) => ShakeVar a -> Rules ()
shakeVarRule var@(ShakeVar _ _ act) = var !> act

-- OLD, plz inline
(!>) :: (Show a) => ShakeVar a -> Action a -> Rules ()
(!>) (ShakeVar _ nm _) m = (== nm) ?> \ out -> do
        r <- m
        writeFileChanged out $ show r

readShakeVar :: (Read a) => ShakeVar a -> Action a
readShakeVar (ShakeVar v nm _) = do
        need [nm]
        txt <- readFile' $ nm
        return $ read txt
{-
writeShakeVar :: (Show a) => ShakeVar a -> a -> Action ()
writeShakeVar (ShakeVar _ nm _) = do
        writeFileChanged nm . show
-}
-------------------------------------------------------

--newtype FPGM a = FPGM { runFPGM :: Action (Either String a) }
