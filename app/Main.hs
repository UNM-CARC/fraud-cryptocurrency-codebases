module Main where

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Text    as Txt
import qualified Data.Text.IO as Txt
import qualified Data.ByteString.Char8 as C
import qualified Data.Digest.Pure.MD5 as MD
import qualified Data.CSV as CSV

import           Text.ParserCombinators.Parsec
import           Data.List
import           Data.Either
import           Data.Csv
import           Data.HexString
import           System.IO
import           Control.Monad
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Zip
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

main :: IO ()
main = do
  input <- fmap Txt.lines $ Txt.readFile "misc/names.csv"
  let clean = fmap (\x -> fmap Txt.unpack x) $ 
              fmap (\x -> (Txt.splitOn $ (Txt.pack ",") ) x) input
  let tmp   = splitEvery 3 $ fmap (filter (/= '\n') 
            . filter (/= '\r')) $ concat clean
  let name = head (head tmp)
  --cloneRepo $ head tmp
  --print $ head tmp
  --getDirectoryContents "/tmp/BTC"
  dirlist1 <- traverseDir (\_ -> True) (\fs f -> pure (f : fs)) [] ("/tmp/" ++ name)
  dirlist2 <- traverseDir (\_ -> True) (\fs f -> pure (f : fs)) [] ("/tmp/BTC-new")
  let dirs = dirlist1
  let dirs2 = dirlist2

  let inter00 = filterFileType "/."    dirs
  let inter01 = filterFileType ".png"  inter00
  let inter02 = filterFileType ".jpg"  inter01
  let inter03 = filterFileType ".svg"  inter02
  let inter04 = filterFileType ".ico"  inter03
  let inter05 = filterFileType ".bmp"  inter04
  let inter06 = filterFileType ".json" inter05
  let inter07 = filterFileType ".icns" inter06
  let inter08 = filterFileType ".raw"  inter07
  let inter09 = filterFileType ".dat"  inter08

  let inter00a = filterFileType "/."    dirs2
  let inter01a = filterFileType ".png"  inter00a
  let inter02a = filterFileType ".jpg"  inter01a
  let inter03a = filterFileType ".svg"  inter02a
  let inter04a = filterFileType ".ico"  inter03a
  let inter05a = filterFileType ".bmp"  inter04a
  let inter06a = filterFileType ".json" inter05a
  let inter07a = filterFileType ".icns" inter06a
  let inter08a = filterFileType ".raw"  inter07a
  let inter09a = filterFileType ".dat"  inter08a


  out <- traverse Txt.readFile inter09
  out2 <- traverse Txt.readFile inter09a

--First iteration
  let n = map (length . Txt.lines) out
  let m = out
  let x = map (MD.md5 . BLU.fromString . Txt.unpack) m
  let o = map (toText . fromBytes . MD.md5DigestBytes) x
  let z = zip3 inter09 o n
  LB.writeFile ("data/" ++ name ++ ".csv") $ encode z

-- Second iteration
  let n2 = map (length . Txt.lines) out2
  let m2 = out2
  let x2 = map (MD.md5 . BLU.fromString . Txt.unpack) m2
  let o2 = map (toText . fromBytes . MD.md5DigestBytes) x2
  let z2 = zip3 inter09a o2 n2
  LB.writeFile ("data/BTC-new.csv") $ encode z2

--  print z

  csv <- parseFromFile CSV.csvFile ("data/" ++ name ++ ".csv")
  csv2 <- parseFromFile CSV.csvFile ("data/BTC-new.csv")

  let p = rights [csv]
  let p2 = rights [csv2]

  let m = compareCoinHashes (concat p) (concat p2) 0.0
  print m

--  print p
