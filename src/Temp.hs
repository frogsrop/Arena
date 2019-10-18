{-# LANGUAGE Arrows              #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Temp
  ( temp
  ) where

import           Arena
import           Control.Monad.Cont         (lift)
import           Control.Monad.Reader       (ask)
import           Control.Monad.Trans.Writer
import           Data.List
import           DatasTypesClasses
import           GHC.IO                     (liftIO)
import           GHC.IO.Unsafe              (unsafePerformIO)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Node

temp :: IO ()
temp = putStrLn "123"

instance Functor (MyMaybeT m) where
  fmap = undefined

instance Applicative (MyMaybeT m) where
  pure = undefined
  (<*>) = undefined

newtype MyMaybeT m a =
  MyMaybeT
    { runMyMaybeT :: m (Maybe a)
    }

instance Monad m => Monad (MyMaybeT m) where
  return :: a -> MyMaybeT m a
  return x = MyMaybeT $ return $ return x
  (>>=) :: MyMaybeT m a -> (a -> MyMaybeT m b) -> MyMaybeT m b
  MyMaybeT g >>= f =
    MyMaybeT $
    g >>= \case
      Just b -> runMyMaybeT (f b)
      Nothing -> return Nothing
--  >>= :: m a -> ( a -> m b) -> m b
