module Main where

import qualified Data.ByteString as B
import qualified Data.Text    as Txt
import qualified Data.Text.IO as Txt
import qualified Data.ByteString.Char8 as C

import           Data.Bits
import           Data.Char
import           Data.List
import           Data.ByteString (ByteString)
import           Data.Word8
import           System.IO
import           Control.Monad
import           Control.Applicative
import           System.Process

import Lib

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

main :: IO ()
main = do
  input <- fmap Txt.lines $ Txt.readFile "misc/names.csv"
  let clean = fmap (\x -> fmap Txt.unpack x) $ fmap (\x -> (Txt.splitOn $ (Txt.pack ",") ) x) input
  let tmp = splitEvery 3 $ fmap (filter (/= '\n') . filter (/= '\r')) $ concat clean
  print tmp
  
