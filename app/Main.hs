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
import           Data.List
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

  let inter00 = fmap (mfilter (\x -> if isInfixOf "/."    x then False else True)) dirlist
  let inter01 = fmap (mfilter (\x -> if isInfixOf ".png"  x then False else True)) inter00
  let inter02 = fmap (mfilter (\x -> if isInfixOf ".jpg"  x then False else True)) inter01
  let inter03 = fmap (mfilter (\x -> if isInfixOf ".svg"  x then False else True)) inter02
  let inter04 = fmap (mfilter (\x -> if isInfixOf ".ico"  x then False else True)) inter03
  let inter05 = fmap (mfilter (\x -> if isInfixOf ".bmp"  x then False else True)) inter04
  let inter06 = fmap (mfilter (\x -> if isInfixOf ".json" x then False else True)) inter05
  let inter07 = fmap (mfilter (\x -> if isInfixOf ".icns" x then False else True)) inter06
  let inter08 = fmap (mfilter (\x -> if isInfixOf ".raw"  x then False else True)) inter07
  let inter09 = fmap (mfilter (\x -> if isInfixOf ".dat"  x then False else True)) inter08
  
--  inter08
  let out = fmap (traverse Txt.readFile) inter09 -- dirlist -- fun dirlist []
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

  
