module Util where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text    as Txt
import qualified Data.Text.IO as Txt
import qualified Data.ByteString.Char8 as C

import           Data.List
import           Data.Char
import           Data.String
import           System.IO
import           Control.Monad
import           Control.Applicative
import           System.Process
import           Data.Digest.Pure.MD5
--import           Control.Monad (foldM)
import           System.Directory (doesDirectoryExist, listDirectory) 
import           System.FilePath ((</>), FilePath)
import           System.FilePath.Posix
import           Control.Monad.Extra (partitionM)
-- import           Text.Regex.TDFA
--import           Text.Regex.PCRE
import           System.Directory.Tree (
                   AnchoredDirTree(..), DirTree(..),
                   filterDir, readDirectoryWith
                   )
import           System.FilePath (takeExtension)

import Lib
--import ParseTrees

-- C style comment removal :: https://stackoverflow.com/questions/7904805/haskell-program-to-remove-comments
stripComments :: String -> String
stripComments [] = []
stripComments ('/':'/':xs) = inComment xs 
stripComments ('/':'*':xs) = inMultiComment xs
stripComments ('\"':xs) = '\"' : inString xs
stripComments (x:xs) = x : stripComments xs

inComment :: String -> String
inComment [] = []
inComment ('\n':xs) = stripComments xs
inComment (_:xs) = inComment xs

inMultiComment :: String -> String
inMultiComment [] = []
inMultiComment ('*':'/':xs) = stripComments xs
inMultiComment (_:xs) = inMultiComment xs

inString :: String -> String
inString [] = []
inString ('\"':xs) = '\"' : stripComments xs
inString (x:xs) = x : inString xs

-- Compare all the hashes of one coin against another and return similarity
-- lx and ly are lengths of xs and ys respectively.
--
-- ** For now we no longer care about a percentage comparing the two as we
--    are more concerned with which repository is older. We will look into
--    using git log for this.
--
--compareCoinHashes :: [[String]] -> [[String]] -> Int -> Int -> ([[String]], Float) 
--                                                            -> ([[String]], Float)
compareCoinHashes :: [[String]] -> [[String]] -> Int -> Int -> ([[String]], Int) 
                                                            -> ([[String]], Int)
--compareCoinHashes []     ys lx ly acc = (fst acc, if lx > ly then snd acc / fromIntegral lx else snd acc / fromIntegral ly)
compareCoinHashes []     ys lx ly acc = (fst acc, snd acc)
compareCoinHashes (x:xs) ys lx ly acc = compareCoinHashes xs ys lx ly
  (fst acc ++ fst fun, snd acc + snd fun)
--  (acc ++ (foldr (\y a -> if head y ==
--                            head x then (y : fst a, snd a + 1)
--                                     else (fst a, snd a)) ([], 0.0) ys))
  where
    fun = (foldr (\y a -> if head y ==
                             head x then (y : fst a, snd a + 1)
                                    else (fst a, snd a)) ([], 0) ys)

filterFileType :: String -> [String] -> [String]
filterFileType s xs = filter (\x -> if isInfixOf s x then True else False) xs
--filterFileType s xs = filter (\x -> if isInfixOf s x then False else True) xs -- Old

-- Remove duplicate files
compressFiles :: [[String]] -> [[String]]
compressFiles files = foldr (\b a -> if a == []
  then b:a else if (head b) == (head $ head a)
                  then (head a ++ [last b]) : tail a
                    else b : a) [] files

-- https://stackoverflow.com/questions/51712083/recursively-search-directories-for-all-files-matching-name-criteria-in-haskell
traverseDir :: (FilePath -> Bool) -> (b -> FilePath -> IO b) -> b -> FilePath -> IO b
traverseDir validDir transition =
  let go state dirPath =
        do names <- listDirectory dirPath
           let paths = map (dirPath </>) names
           (dirPaths, filePaths) <- partitionM doesDirectoryExist paths
           state' <- foldM transition state filePaths -- process current dir
           foldM go state' (filter validDir dirPaths) -- process subdirs
           in go

cloneRepos :: [[String]] -> IO b
cloneRepos (x:xs) = do
  cloneRepo x
  print $ head $ tail x
  cloneRepos xs

--allFiles :: String -> IO (DirTree FilePath)
--allFiles dir = do
--    _:/tree <- readDirectoryWith return dir
--    let x = filterDir prd tree
--    --print x
--    return x
--  where prd (Dir ('.':_) _) = False
--        prd _ = True

-- Split a list of a into a list of lists every n values.
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
  where (as,bs) = splitAt n xs

-- Clone a given repository based upon a list of three values:
-- ["BTC", "bitcoin", "https..."]
cloneRepo :: [String] -> IO ()
cloneRepo coin = do
  let str = "git clone --recursive " ++ last coin ++ " /tmp/" ++ head coin
  (errc, out', err') <- readCreateProcessWithExitCode (shell str) []
  print $ head coin


writeDataToFile :: FilePath -> String -> IO ()
writeDataToFile file dat = do
  let fileNew = "data/" ++ (takeBaseName file) ++ ".csv"
  (errc, out, err) <- readCreateProcessWithExitCode (shell ("touch " ++ fileNew)) []
  h <- openFile fileNew WriteMode
  hPutStr h dat
  hClose h

writeTreeToFile :: FilePath -> String -> String -> IO ()
writeTreeToFile file tree num = do
  let fileNew = "asts/" ++ (takeBaseName file) ++ num ++ ".ast"
  (errc, out, err) <- readCreateProcessWithExitCode (shell ("touch " ++ fileNew)) []
  h <- openFile fileNew WriteMode
  hPutStr h tree
  hClose h

convertToCSVLine :: (String, String, Int, Int, Int) -> String
convertToCSVLine (a, b, c, d, e) = a ++ "," ++ 
                                   b ++ "," ++ 
                                   (show c) ++ "," ++ 
                                   (show d) ++ "," ++ 
                                   (show e)

