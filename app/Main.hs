module Main where

import qualified Data.ByteString as B
import qualified Data.Text    as Txt
import qualified Data.Text.IO as Txt
import qualified Data.ByteString.Char8 as C

import           Data.Bits
import           Data.Char
import           Data.List
import           Data.ByteString (ByteString)
import           Data.Word8
import           System.IO
import           Control.Monad
import           Control.Applicative
import           System.Process
--import           System.Directory
import           Data.Hash.MD5

import           Control.Monad (foldM)
import           System.Directory (doesDirectoryExist, listDirectory) 
import           System.FilePath ((</>), FilePath)
import           Control.Monad.Extra (partitionM)

import Lib

traverseDir :: (FilePath -> Bool) -> (b -> FilePath -> IO b) -> b -> FilePath -> IO b
traverseDir validDir transition =
  let go state dirPath = 
        do names <- listDirectory dirPath
           let paths = map (dirPath </>) names
           (dirPaths, filePaths) <- partitionM doesDirectoryExist paths
           state' <- foldM transition state filePaths -- process current dir
           foldM go state' (filter validDir dirPaths) -- process subdirs
           in go

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

cloneRepo :: [String] -> IO ()
cloneRepo x = do
  let str = "git clone --recursive " ++ last x ++ " /tmp/" ++ head x
  (errc, out', err') <- readCreateProcessWithExitCode (shell str) []
  print $ head x

--hashFile :: String -> IO ()

--generateTree :: String -> DirTree a
--generateTree prfx = do
--  let str = " /tmp/" ++ prfx
  

main :: IO ()
main = do
  input <- fmap Txt.lines $ Txt.readFile "misc/names.csv"
  let clean = fmap (\x -> fmap Txt.unpack x) $ fmap (\x -> (Txt.splitOn $ (Txt.pack ",") ) x) input
  let tmp = splitEvery 3 $ fmap (filter (/= '\n') . filter (/= '\r')) $ concat clean
  --cloneRepo $ head tmp
  --print $ head tmp
  --getDirectoryContents "/tmp/BTC"
  traverseDir (\_ -> True) (\() path -> print path) () "/tmp/BTC"

  
