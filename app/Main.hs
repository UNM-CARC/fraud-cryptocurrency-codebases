module Main where

--import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Text    as Txt
import qualified Data.Text.IO as Txt
import qualified Data.ByteString.Char8 as C

import           Data.Bits
import           Data.Char
import           Data.List
import           Data.ByteString (ByteString, pack)
import           Data.Word8
import           System.IO
import           Control.Monad
import           Control.Applicative
import           System.Process
import           Data.Digest.Pure.MD5
import           Control.Monad (foldM)
import           System.Directory (doesDirectoryExist, listDirectory) 
import           System.FilePath ((</>), FilePath)
import           System.FilePath.Posix
import           Control.Monad.Extra (partitionM)

import Lib
import Util

--main :: IO [FilePath]
main = do
  input <- fmap Txt.lines $ Txt.readFile "misc/names.csv"
  let clean = fmap (\x -> fmap Txt.unpack x) $ 
              fmap (\x -> (Txt.splitOn $ (Txt.pack ",") ) x) input
  let tmp   = splitEvery 3 $ fmap (filter (/= '\n') 
            . filter (/= '\r')) $ concat clean
  --cloneRepo $ head tmp
  --print $ head tmp
  --getDirectoryContents "/tmp/BTC"
  --traverseDir (\_ -> True) (\() path -> print path) () "/tmp/BTC"
  let dirlist = traverseDir (\_ -> True) (\fs f -> pure (f : fs)) [] 
                ("/tmp/" ++ head (head tmp))
  print $ fmap md5 $ fmap BLU.fromString $ fmap takeBaseName $ concat dirlist
  --generateHashes dirlist 

  
