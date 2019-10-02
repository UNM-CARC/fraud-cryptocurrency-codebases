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
  let name =  "bitcoin0.14"-- head (head tmp)
  let name2 = "MicroBitcoin"
        --"MicroBitcoin"
        -- "bitcoin2"
  --cloneRepo $ head tmp
  --print $ head tmp
  --getDirectoryContents "/tmp/BTC"

  dirlist1 <- traverseDir (\_ -> True) (\fs f -> pure (f : fs)) [] ("/tmp/" ++ name)
  dirlist2 <- traverseDir (\_ -> True) (\fs f -> pure (f : fs)) [] ("/tmp/" ++ name2)
  let dirs = map (\x -> x ++ " ") dirlist1
  let dirs2 = map (\x -> x ++ " ") dirlist2

  let inter = filterFileType ".c "    dirs
           ++ filterFileType ".go "   dirs
           ++ filterFileType ".py "   dirs
           ++ filterFileType ".cpp "  dirs
           ++ filterFileType ".sh "   dirs
           ++ filterFileType ".html " dirs
           ++ filterFileType ".h "    dirs
           ++ filterFileType ".js "   dirs

  let intera = filterFileType ".c "   dirs2
            ++ filterFileType ".go "  dirs2
            ++ filterFileType ".py "  dirs2
            ++ filterFileType ".cpp " dirs2
            ++ filterFileType ".sh "  dirs2
            ++ filterFileType ".h "   dirs2
            ++ filterFileType ".js "  dirs2


  let inter1  = map init inter
  let inter1a = map init intera

--  print inter1

  out <- traverse Txt.readFile inter1 --19
  out2 <- traverse Txt.readFile inter1a --19a

  -- Remove C style comments and whitespace from files.
  let a = map Txt.pack $ map (concat . words) $ map (stripComments . Txt.unpack) out
  let b = map Txt.pack $ map (concat . words) $ map (stripComments . Txt.unpack) out2

  --First iteration
  -- Get number of lines per file.
  let n = map (length . Txt.lines) out
  let m = out

  -- To test on unmodified vs modified use either m or a. (end of first line)
  let x = map (MD.md5 . BLU.fromString . Txt.unpack) m
  let o = map (toText . fromBytes . MD.md5DigestBytes) x
  let z = zip3 o n inter1
  LB.writeFile ("data/" ++ name ++ ".csv") $ encode z

  -- Second iteration
  -- Get number of lines per file.
  let n2 = map (length . Txt.lines) out2
  let m2 = out2

  -- To test on unmodified vs modified use either m2 or b. (end of first line)
  let x2 = map (MD.md5 . BLU.fromString . Txt.unpack) m2
  let o2 = map (toText . fromBytes . MD.md5DigestBytes) x2
  let z2 = zip3 o2 n2 inter1a
  LB.writeFile ("data/" ++ name2 ++ ".csv") $ encode z2

--  print z

  csv <- parseFromFile CSV.csvFile ("data/" ++ name ++ ".csv")
  csv2 <- parseFromFile CSV.csvFile ("data/" ++ name2 ++ ".csv")

  let p  = rights [csv]
  let p2 = rights [csv2]
  let l  = sort (concat p)
  let l2 = sort (concat p2)

  let k = compressFiles l
  let k2 = compressFiles l2

  --print k
  let aa = snd $ compareCoinHashes k k2 ([], 0.0)
  let bb = fst $ compareCoinHashes k k2 ([], 0.0)

  --print (length $ k)
  print bb
  print aa

--  print p
