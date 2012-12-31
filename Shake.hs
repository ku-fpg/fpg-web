module Shake where

import Development.Shake hiding (getDirectoryContents)
import Development.Shake.FilePath

import Control.Concurrent.STM

data ShakeVar a = ShakeVar (TVar (Maybe a)) String

newShakeVar :: String -> IO (ShakeVar a)
newShakeVar nm = do
        v <- atomically $ newTVar $ Nothing
        return $ ShakeVar v nm

infix 1 !>

(!>) :: (Show a) => ShakeVar a -> Action a -> Rules ()
(!>) (ShakeVar _ nm) m = (== nm) ?> \ out -> do
        r <- m
        writeFileChanged out $ show r

readShakeVar :: (Read a) => ShakeVar a -> Action a
readShakeVar (ShakeVar v nm) = do
        need [nm]
        txt <- readFile' $ nm
        return $ read txt

writeShakeVar :: (Show a) => ShakeVar a -> a -> Action ()
writeShakeVar (ShakeVar _ nm) = do
        writeFileChanged nm . show

