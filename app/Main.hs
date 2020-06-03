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

  input <- fmap Txt.lines $ Txt.readFile "misc/repos.csv"
  --input <- fmap Txt.lines $ Txt.readFile "misc/names.csv"
  let clean = fmap (\x -> fmap Txt.unpack x) $
              fmap (\x -> (Txt.splitOn $ (Txt.pack ",") ) x) input
  let tmp   = splitEvery 5 $ fmap (filter (/= '\n')
            . filter (/= '\r')) $ concat clean

  let filtered = filterSelected tmp
  --print (map snd $ transferToTuple tmp)
  --print tmp
  print $ length $ (map snd $ filtered)
  --cloneRepos (\x -> snd x) filtered
  --compareRepos "bitcoin" "bitcoin0.14" 0
  --compareAllBasicRepos 0
  --compareAllRepos
  --compareParseTreesRepos (head args) (head (tail args))
