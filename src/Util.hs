module Util where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text    as Txt
import qualified Data.Text.IO as Txt
import qualified Data.ByteString.Char8 as C

import           Data.List
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
import           System.Directory.Tree (
                   AnchoredDirTree(..), DirTree(..),
                   filterDir, readDirectoryWith
                   )
import           System.FilePath (takeExtension)

import Lib

-- Compare all the hashes of one coin against another and return similarity
compareCoinHashes :: [[String]] -> [[String]] -> Float -> Float
compareCoinHashes []     ys acc = acc / fromIntegral (length ys)
compareCoinHashes (x:xs) ys acc = compareCoinHashes xs ys 
  (acc + (foldr (\y a -> if head y == 
                            head x then a + 1
                                     else a) 0.0 ys))

filterFileType :: String -> [String] -> [String]
filterFileType s xs = filter (\x -> if isInfixOf s x then False else True) xs

--filterFileType :: String -> [String] -> [String]
--filterFileType s xs = filter (\x -> if (map (isInfixOf) [ ".cpp"
--                                                       , ".c"
--                                                       , ".h"
--                                                       , ".sh"
--                                                       , ".go"
--                                                       , ".py"]) $ x then False else True) xs

compressFiles :: [[String]] -> [[String]]
compressFiles files = foldr (\b a -> if a == [] 
  then b:a else if (head b) == (head $ head a) 
                  then (head a ++ [last b]) : tail a 
                    else b : a) [] files

traverseDir :: (FilePath -> Bool) -> (b -> FilePath -> IO b) -> b -> FilePath -> IO b
traverseDir validDir transition =
  let go state dirPath = 
        do names <- listDirectory dirPath
           let paths = map (dirPath </>) names
           (dirPaths, filePaths) <- partitionM doesDirectoryExist paths
           state' <- foldM transition state filePaths -- process current dir
           foldM go state' (filter validDir dirPaths) -- process subdirs
           in go


allFiles :: String -> IO (DirTree FilePath)
allFiles dir = do
    _:/tree <- readDirectoryWith return dir
    let x = filterDir prd tree
    --print x
    return x
  where prd (Dir ('.':_) _) = False
        prd _ = True


splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

cloneRepo :: [String] -> IO ()
cloneRepo coin = do
  let str = "git clone --recursive " ++ last coin ++ " /tmp/" ++ head coin
  (errc, out', err') <- readCreateProcessWithExitCode (shell str) []
  print $ head coin


