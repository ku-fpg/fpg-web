{-# LANGUAGE ScopedTypeVariables, GADTs #-}
module Shake where

import Development.Shake hiding (getDirectoryContents)
import Development.Shake.FilePath
import Control.Applicative hiding ((*>))
import Control.Monad.IO.Class
import qualified Language.KURE as KURE
import Language.KURE hiding (apply)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import KURE

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

-------------------------------------------------------

--newtype FPGM a = FPGM { runFPGM :: Action (Either String a) }

newtype FPGM a = FPGM { runFPGM :: IO (FPGMResult a) }

data FPGMResult a
        = FPGMResult a
        | FPGMFail String
        | forall r . FPGMAction (Action r) (r -> FPGM a)

applyFPGM' :: forall a b . Translate Context FPGM a b -> a -> Action b
applyFPGM' t a = do

        let loop (FPGMResult a) = return a
            loop (FPGMFail msg) =  fail $ "applyFPGM " ++ msg
            loop (FPGMAction act rest) = do
                              res <- act
                              run (rest res)

            run m = do res <- traced "apply-yah" $ runFPGM m
                       loop res

        run $ KURE.apply t (Context []) a

liftActionFPGM :: Action a -> FPGM a
liftActionFPGM m = FPGM $ return $ FPGMAction  m return

type T a b = Translate Context FPGM a b
type R a   = T a a

instance Monad FPGM where

        return = FPGM . return . FPGMResult

        m1 >>= k = FPGM $ do
                r <- runFPGM m1
                let f (FPGMResult a) = runFPGM (k a)
                    f (FPGMFail msg) = return (FPGMFail msg)
                    f (FPGMAction act rest) = return $ FPGMAction act (\ a -> rest a >>= k)
                f r

        fail = FPGM . return . FPGMFail

instance Functor FPGM where
        fmap f m = pure f <*> m

instance Applicative FPGM where
        pure a = return a
        af <*> aa = af >>= \ f -> aa >>= \ a -> return (f a)


instance MonadCatch FPGM where
        catchM m1 handle = FPGM $ do
                r <- runFPGM m1
                let f (FPGMResult a) = return (FPGMResult a)
                    f (FPGMFail msg) = runFPGM (handle msg)
                    f (FPGMAction act rest) = return (FPGMAction act rest)
                f r
instance MonadIO FPGM where
        liftIO m = FPGM (FPGMResult <$> m)
