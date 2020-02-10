module Basic where

import qualified Data.Text    as Txt
import qualified Data.Text.IO as Txt

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Char8 as C
import qualified Data.Digest.Pure.MD5 as MD
import qualified Data.CSV as CSV

import           System.Directory (doesDirectoryExist, listDirectory)
import           Text.ParserCombinators.Parsec
import           Numeric
import           Data.List
import           Data.Char
import           Data.Either
import           Data.Csv
import           Data.HexString
import           System.IO
import           Control.Applicative
import           Control.Monad
import           System.Environment
import           System.FilePath ((</>), FilePath)
import           System.FilePath.Posix
import           Control.Monad.Extra (partitionM)
import           Data.Traversable (traverse)
import           System.Directory.Tree (
                   AnchoredDirTree(..), DirTree(..),
                   filterDir, readDirectoryWith
                   )

import Util

compareAllBasicRepos :: Int -> IO ()
compareAllBasicRepos flag = do
  input <- fmap Txt.lines $ Txt.readFile "misc/testset.csv"
  let clean = fmap (\x -> fmap Txt.unpack x) $
              fmap (\x -> (Txt.splitOn $ (Txt.pack ",") ) x) input
  let tmp   = splitEvery 3 $ fmap (filter (/= '\n')
            . filter (/= '\r')) $ concat clean
  let repos = map takeFileName $ concat $ map (tail . tail) tmp
  foldRepos repos flag
  --return tmp

foldRepos :: [String] -> Int -> IO ()
foldRepos    []  _   = return ()
foldRepos (x:xs) num = do
  mapM (\y -> compareRepos x y num) xs
  foldRepos xs num

-- Compare repositories both original and after removal of whitespace and comments.
compareRepos :: String -> String -> Int -> IO ()
compareRepos name1 name2 flag = do

  dirlist1 <- traverseDir (\_ -> True) (\fs f -> pure (f : fs)) [] ("/wheeler/scratch/khaskins/" ++ name1)
  dirlist2 <- traverseDir (\_ -> True) (\fs f -> pure (f : fs)) [] ("/wheeler/scratch/khaskins/" ++ name2)
  let dirs  = map (\x -> x ++ " ") dirlist1
  let dirs2 = map (\x -> x ++ " ") dirlist2

  let inter  = filterFileType ".cpp "  dirs
  let intera = -- filterFileType ".c "   dirs2
            -- ++ filterFileType ".go "  dirs2
            -- ++ filterFileType ".py "  dirs2
               filterFileType ".cpp " dirs2
            -- ++ filterFileType ".sh "  dirs2
            -- ++ filterFileType "html " dirs2
            -- ++ filterFileType ".h "   dirs2
            -- ++ filterFileType ".js "  dirs2

  let inter1  = map init inter
  let inter1a = map init intera

  -- print inter1

  out <- traverse Txt.readFile inter1 --19
  out2 <- traverse Txt.readFile inter1a --19a

  -- Remove C style comments and whitespace from files.
  let a = map Txt.pack $ map (concat . words) $ map (stripComments . Txt.unpack) out
  let b = map Txt.pack $ map (concat . words) $ map (stripComments . Txt.unpack) out2

  --First iteration
  -- Get number of lines per file.
  let n = map (length . Txt.lines) out
  let m = out

  case flag of
    -- Test unmodified
    0 -> do
      -- To test on unmodified vs preprocessed use either m or a. (end of first line)
      let x = map (MD.md5 . BLU.fromString . Txt.unpack) m
      let o = map (toText . fromBytes . MD.md5DigestBytes) x
      let z = zip3 o n inter1
      LB.writeFile ("inter/" ++ name1 ++ ".csv") $ encode z

      -- Second iteration
      -- Get number of lines per file.
      let n2 = map (length . Txt.lines) out2
      let m2 = out2

      -- To test on unmodified vs preprocessed use either m2 or b. (end of first line)
      let x2 = map (MD.md5 . BLU.fromString . Txt.unpack) m2
      let o2 = map (toText . fromBytes . MD.md5DigestBytes) x2
      let z2 = zip3 o2 n2 inter1a
      LB.writeFile ("inter/" ++ name2 ++ ".csv") $ encode z2

      csv <- parseFromFile CSV.csvFile ("inter/" ++ name1 ++ ".csv")
      csv2 <- parseFromFile CSV.csvFile ("inter/" ++ name2 ++ ".csv")

      let p  = rights [csv]
      let p2 = rights [csv2]
      let l  = sort (concat p)
      let l2 = sort (concat p2)

      let k = compressFiles l
      let k2 = compressFiles l2

      let yy = compareCoinHashes k k2 (length k) (length k2) ([], 0)
      let aa = snd yy
      let bb = fst yy

      let dat = convertToCSVLine (name1, name2, length k, length k2, length bb)
      writeDataToFile "level1" (dat ++ "\n")

      --print $ map last bb -- Only print file names here
      --
      --print $ Numeric.showFFloat Nothing aa ""
      --
      --print $ "Length of " ++ name1 ++ ": " ++ (show $ length k)
      --print $ "Length of " ++ name2 ++ ": " ++ (show $ length k2)
      --print $ "Number of matching files " ++ (show $ length bb)

    -- Test preprocessed
    1 -> do
      -- To test on unmodified vs preprocessed use either m or a. (end of first line)
      let x = map (MD.md5 . BLU.fromString . Txt.unpack) a
      let o = map (toText . fromBytes . MD.md5DigestBytes) x
      let z = zip3 o n inter1
      LB.writeFile ("inter/" ++ name1 ++ "pre.csv") $ encode z

      -- Second iteration
      -- Get number of lines per file.
      let n2 = map (length . Txt.lines) out2
      let m2 = out2

      -- To test on unmodified vs preprocessed use either m2 or b. (end of first line)
      let x2 = map (MD.md5 . BLU.fromString . Txt.unpack) b
      let o2 = map (toText . fromBytes . MD.md5DigestBytes) x2
      let z2 = zip3 o2 n2 inter1a
      LB.writeFile ("inter/" ++ name2 ++ "pre.csv") $ encode z2

      csv <- parseFromFile CSV.csvFile ("inter/" ++ name1 ++ "pre.csv")
      csv2 <- parseFromFile CSV.csvFile ("inter/" ++ name2 ++ "pre.csv")

      let p  = rights [csv]
      let p2 = rights [csv2]
      let l  = sort (concat p)
      let l2 = sort (concat p2)

      let k = compressFiles l
      let k2 = compressFiles l2

      let yy = compareCoinHashes k k2 (length k) (length k2) ([], 0)
      let aa = snd yy
      let bb = fst yy

      let dat = convertToCSVLine (name1, name2, length k, length k2, length bb)
      writeDataToFile "level2" (dat ++ "\n")

      --print $ map last bb -- Only print file names here
      --
      ----print $ Numeric.showFFloat Nothing aa ""
      --
      --print $ "Length of " ++ name1 ++ ": " ++ (show $ length k)
      --print $ "Length of " ++ name2 ++ ": " ++ (show $ length k2)
      --print $ "Number of matching files " ++ (show $ length bb)

    _ -> error "invalid flag value..."
