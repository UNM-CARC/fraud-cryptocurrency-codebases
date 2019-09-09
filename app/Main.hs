module Main where

--import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Text    as Txt
import qualified Data.Text.IO as Txt
import qualified Data.ByteString.Char8 as C

--import           Data.Bits
--import           Data.Char
--import           Data.List
--import           Data.ByteString (ByteString, pack)
--import           Data.Word8
import           System.IO
import           Control.Monad
import           Control.Applicative
--import           System.Process
import           Data.Digest.Pure.MD5
import           Control.Monad (foldM)
import           System.Directory (doesDirectoryExist, listDirectory) 
import           System.FilePath ((</>), FilePath)
import           System.FilePath.Posix

import           System.Directory.Tree (
                   AnchoredDirTree(..), DirTree(..),
                   filterDir, readDirectoryWith
                   )
import           Control.Monad.Extra (partitionM)
import           Data.Traversable (traverse)


import Lib
import Util


{-
--fun :: [FilePath] -> [(String, String)] -> [(String, String)]
fun []       acc = print acc
fun (d:dirs) acc = do
  (errc, out', err') <- readCreateProcessWithExitCode (shell ("md5sum " ++ d)) []
  fun dirs (acc ++ [(takeBaseName d, out')])
-}

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
  let out = traverse Txt.readFile dirlist -- fun dirlist []
  --let out = allFiles ("/tmp/" ++ head (head tmp))
  --let x = foldr (\y x -> (md5 . BLU.fromString)) out
  --dirlist
  --putStrLn out
  out
  --where
    --fun :: FilePath -> IO (String, String)
     

 -- print $ fmap md5 $ fmap BLU.fromString $ fmap takeBaseName dirlist
  --generateHashes dirlist 

  
