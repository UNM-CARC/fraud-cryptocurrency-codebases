module Util where

import qualified Data.ByteString.Lazy as B
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
cloneRepo coin = do
  let str = "git clone --recursive " ++ last coin ++ " /tmp/" ++ head coin
  (errc, out', err') <- readCreateProcessWithExitCode (shell str) []
  print $ head coin

--hashFile :: String -> IO ()

generateHashes :: [FilePath] -> [(String, B.ByteString)]-> [(String, B.ByteString)]
generateHashes []          acc = acc
generateHashes (d:dirlist) acc = do
  --foldl (\x y -> x ++ [(concat $ y ,md5 y)]) [] dirlist
  let str = "md5sum " ++ d
  (errc, out', err') <- readCreateProcessWithExitCode (shell str) []
  generateHashes dirlist (acc ++ [(takeBaseName d, B.pack out')])
