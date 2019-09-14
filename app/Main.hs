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
import           Data.Csv
import           Data.HexString
--import           Data.ByteString (ByteString, pack)
--import           Data.Word8
import           System.IO
import           Control.Monad
import           Control.Applicative
--import           System.Process
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
  dirlist <- traverseDir (\_ -> True) (\fs f -> pure (f : fs)) [] 
                  ("/tmp/" ++ name)
  let dirs = dirlist

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

  out <- traverse Txt.readFile inter09
  let n = map (length . Txt.lines) out
  let m = out
  let x = map (MD.md5 . BLU.fromString . Txt.unpack) m
  let o = map (toText . fromBytes . MD.md5DigestBytes) x
  let z = zip3 inter09 o n
  LB.writeFile (name ++ ".csv") $ encode z
  print z
