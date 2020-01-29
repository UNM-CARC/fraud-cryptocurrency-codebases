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
  let flag = 1

  input <- fmap Txt.lines $ Txt.readFile "misc/testset.csv"
  let clean = fmap (\x -> fmap Txt.unpack x) $
              fmap (\x -> (Txt.splitOn $ (Txt.pack ",") ) x) input
  let tmp   = splitEvery 3 $ fmap (filter (/= '\n')
            . filter (/= '\r')) $ concat clean
  --print tmp
  --cloneRepos tmp
  compareRepos "bitcoin" "bitcoin0.14" 0
