module Main where

import qualified Data.Text    as Txt
import qualified Data.Text.IO as Txt

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
import           Control.Monad.Zip
import           System.Environment
import           System.FilePath ((</>), FilePath)
import           System.FilePath.Posix
import           Control.Monad.Extra (partitionM)
import           Data.Traversable (traverse)

import Lib
import Util
import ParseTrees
import Basic

main :: IO ()
main = do
  args <- getArgs
  let flag = 1
  handle <- openFile "/wheeler/scratch/khaskins/fraud-cryptocurrency-codebases/misc/names.csv" ReadMode
  hSetEncoding handle utf8_bom
  input <- fmap Txt.lines $ Txt.hGetContents handle

  --input <- fmap Txt.lines $ Txt.readFile "misc/repos.csv"
  --input <- fmap Txt.lines $ Txt.readFile "/wheeler/scratch/khaskins/fraud-cryptocurrency-codebases/misc/repos.csv"
  --input <- fmap Txt.lines $ Txt.readFile "/wheeler/scratch/khaskins/fraud-cryptocurrency-codebases/misc/names.csv"
  let clean = fmap (\x -> fmap Txt.unpack x) $
              fmap (\x -> (Txt.splitOn $ (Txt.pack ",") ) x) input
  hClose handle
  let tmp   = splitEvery 3 $ fmap (filter (/= '\n')
            . filter (/= '\r')) $ concat clean
  --print tmp
  --print $ map (takeBaseName . last) tmp

  --let filtered = filterSelected tmp
  let filtered = filterRepoLinks tmp
  --print (map snd $ transferToTuple tmp)
  --print $ length $ (map snd $ filtered)
  --print filtered
  cloneRepos filtered
  pruneRepos filtered
  --getAllTags
  --compareRepos "bitcoin" "bitcoin0.14" 0
  --compareAllBasicRepos 0
  --compareAllRepos
  --compareParseTreesRepos (head args) (head (tail args))
