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

--import Lib
import Util
import PBSBuild
import ParseTrees
import Basic

main :: IO ()
main = do
  args <- getArgs
  if length args > 0 then
    case head args of
    "0" -> do
      handle <- openFile "/wheeler/scratch/khaskins/fraud-cryptocurrency-codebases/misc/names.csv" ReadMode
      hSetEncoding handle utf8_bom
      input <- fmap Txt.lines $ Txt.hGetContents handle

      let clean = fmap (\x -> fmap Txt.unpack x) $
                  fmap (\x -> (Txt.splitOn $ (Txt.pack ",") ) x) input
      hClose handle
      let tmp   = splitEvery 3 $ fmap (filter (/= '\n')
                . filter (/= '\r')) $ concat clean

      -- Filter all coins which actually have repo links.
      let filtered = filterRepoLinks tmp
      -- Clone all of the links filtered from previous line.
      cloneRepos filtered
      -- Remove all repos which do not contain C++ code.
      pruneRepos filtered
--    "1" -> do
--      filter

    "2" ->
      -- Get all tags from every C++ repo and generate copies of each for each tag
      -- in a new repo <repo>-tag
      getAllTags

--      "3" -> do

    "4" -> do
      let subset = head (tail args)
      let hyp    = head (tail (tail args))
      -- First level of comparison: No modification to source code.
      compareAllBasicRepos 0 hyp
      -- Second level of comparison: Remove C style comments and whitespace.
      compareAllBasicRepos 1  hyp
      -- Compare parse trees.
      compareAllParseTreeRepos hyp
    else do
      putStrLn ""
      putStrLn "No arguments were provided:"
      putStrLn ""
      putStrLn "Usage:"
      putStrLn "       main 0 -> Clone and remove non c++ repos using names.csv"
      putStrLn "       main 1 -> Further filter coins via comparisons to top 10 major coins"
      putStrLn "       main 2 -> Create <REPO>-tags directory and copy each coin,"
      putStrLn "                 giving one major version per year"
      putStrLn "       main 3 -> Generate PBS scripts given "
      putStrLn "       main 4    <subset num(auto-generated via PBSBuild)> <hypothesis(1,2,3)>"
      putStrLn "              -> Run all comparison tests on a given subset of repositories"
