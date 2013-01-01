module Shake where

import Development.Shake hiding (getDirectoryContents)
import Development.Shake.FilePath
import Control.Applicative hiding ((*>))
import Control.Monad.IO.Class
import qualified Language.KURE as KURE
import Language.KURE hiding (apply)

import Control.Concurrent.STM

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

newtype FPGM a = FPGM { runFPGM :: Action (Either String a) }

applyFPGM' :: Translate Context FPGM a b -> a -> Action b
applyFPGM' t a = do
        res <- runFPGM $ KURE.apply t (Context []) a
        case res of
          Left msg -> error $ "applyFPGM " ++ msg
          Right a -> return a

liftActionFPGM :: Action a -> FPGM a
liftActionFPGM = FPGM . fmap Right

type T a b = Translate Context FPGM a b
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
