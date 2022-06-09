{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Semigroup ((<>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import Data.Text (Text)
import Data.IORef
import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)
import Lucid

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
  return  = MaybeT . return . Just

  -- The signature of (>>=), specialized to MaybeT m:
  -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  x >>= f = MaybeT $ do maybe_value <- runMaybeT x
                        case maybe_value of
                           Nothing    -> return Nothing
                           Just value -> runMaybeT $ f value

main :: IO ()
main = putStrLn "Hello"