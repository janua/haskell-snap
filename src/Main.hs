{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import Control.Monad
import Data.ByteString

import Control.Concurrent.MVar (newMVar, MVar, modifyMVar)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
  hitCounter <- newMVar 0
  quickHttpServe $ site hitCounter

site :: MVar Int -> Snap ()
site n =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          , ("site/:site", redirectHandler)
          , ("counter", counterHandler n)
          ] <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param


redirectHandler :: Snap ()
redirectHandler = getParam "site" >>= maybe (writeBS "Error") redirect . resolveDestination


resolveDestination :: Maybe ByteString -> Maybe ByteString
resolveDestination d = case d of
	Just "google"	-> Just "http://www.google.com"
	Just "yahoo"	-> Just "http://www.yahoo.uk"
	_			        -> Nothing


counterHandler :: MVar Int -> Snap ()
counterHandler n = do
  i <- liftIO $ incrementCounter n
  writeBS $ C.pack $ show i

incrementCounter :: MVar Int -> IO Int
incrementCounter n = modifyMVar n $ \s -> return ( s+ 1 , s + 1)
