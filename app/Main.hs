module Main where

--import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Text    as Txt
import qualified Data.Text.IO as Txt
import qualified Data.ByteString.Char8 as C
--import qualified Crypto.Hash as CH
import qualified Data.Digest.Pure.MD5 as MD

--import           Data.Bits
--import           Data.Char
--import           Data.List
--import           Data.ByteString (ByteString, pack)
--import           Data.Word8
import           System.IO
import           Control.Monad
import           Control.Applicative
--import           System.Process
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

--readFiles dirs = case 
--
substring :: String -> String -> Bool
substring (x:xs) [] = False
substring xs ys
    | prefix xs ys = False
    | substring xs (tail ys) = False
    | otherwise = True

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys


--main :: IO [MD5Digest]
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
  let int = fmap (mfilter (\x -> substring "/." x)) dirlist
--  int
  let out = fmap (traverse Txt.readFile) int -- dirlist -- fun dirlist []
  --let out = traverse (Txt.readFile) $ allFiles ("/tmp/" ++ head (head tmp))
  --let out = readFiles $ allFiles ("/tmp/" ++ head (head tmp))
  let x = fmap (fmap (MD.md5 . BLU.fromString . Txt.unpack)) $ join out
  --let x = fmap (fmap (MD.md5 . BLU.fromString . Txt.unpack)) $ out
  --dirlist
  --putStrLn out
  x
    --fun :: FilePath -> IO (String, String)
     

 -- print $ fmap md5 $ fmap BLU.fromString $ fmap takeBaseName dirlist
  --generateHashes dirlist 

  
