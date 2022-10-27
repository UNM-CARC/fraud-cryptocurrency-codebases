module Basic where

import qualified Data.Text    as Txt
import qualified Data.Text.IO as Txt

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Char8 as C
import qualified Data.Digest.Pure.MD5 as MD
import qualified Data.CSV as CSV

import           System.Directory (doesDirectoryExist, listDirectory)
--import           Text.ParserCombinators.Parsec
import           Numeric
import           Data.List
import           Data.Char
import           Data.Either
import           Data.Csv
import           Data.HexString
import           System.IO
import           System.IO.Error
import           Control.Applicative
import           Control.Monad
import           Control.Exception
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

--tryJustReadFile :: FilePath -> String
--tryJustReadFile filePath = do
--  eitherExceptionFile <- tryJust handleReadFile (readFile filePath)
--  case eitherExceptionFile of
--   Left  er   -> putStrLn er
--   Right file -> file
--  where
--    handleReadFile :: IOError -> Maybe String
--    handleReadFile er
--      | isDoesNotExistError er = Just "readFile: does not exist"
--      | isPermissionError   er = Just "readFile: permission denied"
--      | otherwise              = Nothing

-- Compare all the hashes of one coin against another and return similarity
-- lx and ly are lengths of xs and ys respectively.
--
-- ** For now we no longer care about a percentage comparing the two as we
--    are more concerned with which repository is older. We will look into
--    using git log for this.
--
--compareCoinHashes :: [[String]] -> [[String]] -> Int -> Int -> ([[String]], Float)
--                                                            -> ([[String]], Float)
compareCoinHashes :: [[String]] -> [[String]] -> Int -> Int -> ([[String]], Int)
                                                            -> ([[String]], Int)
--compareCoinHashes []     ys lx ly acc = (fst acc, if lx > ly then snd acc / fromIntegral lx else snd acc / fromIntegral ly)
compareCoinHashes []     ys lx ly acc = (fst acc, snd acc)
compareCoinHashes (x:xs) ys lx ly acc = compareCoinHashes xs ys lx ly
  (fst acc ++ fst fun, snd acc + snd fun)
  where
    fun = (foldr (\y a -> if head y ==
                             head x then (y : fst a, snd a + 1)
                                    else (fst a, snd a)) ([], 0) ys)


writeBasicFile :: String -> String -> String -> String -> String -> IO ()
writeBasicFile repo1 repo2 dat hypothesis experiment =
  if "-tags" `intersect` (last $ init $ splitPath repo2) == "-tags" 
    && "-tags" `intersect` (last $ init $ splitPath repo1) == "-tags" then
    -- if both repo 1 and 2 are tagged repos, build new name for writing
    -- including the actual name of the coin.
    writeDataToFile ((filter (/= '/') $ last $ init $ splitPath repo1) 
                    ++ "-" ++ 
                    (filter (/= '/') $ last $ splitPath repo1)) 
                    ((filter (/= '/') $ last $ init $ splitPath repo2) 
                    ++ "-" ++ 
                    (filter (/= '/') $ last $ splitPath repo2))
                    dat hypothesis experiment
  else if "-tags" `intersect` (last $ init $ splitPath repo2) == "-tags" then
    writeDataToFile (takeBaseName repo1) 
                    ((filter (/= '/') $ last $ init $ splitPath repo2)
                    ++ "-" ++ 
                    (filter (/= '/') $ last $ splitPath repo2))
                    dat hypothesis experiment
  else
    writeDataToFile (takeBaseName repo1) (takeBaseName repo2) dat 
                    hypothesis experiment

convertToBasicCSVLine :: String -> String -> Int -> Int -> Int -> String
convertToBasicCSVLine repo1 repo2 lengthA lengthB overlap =
  if "-tags" `intersect` (last $ init $ splitPath repo2) == "-tags" && 
     "-tags" `intersect` (last $ init $ splitPath repo1) == "-tags" then
    -- if both repo 1 and 2 are tagged repos, build new name for writing
    -- including the actual name of the coin.
    convertToCSVLine (((filter (/= '/') $ last $ init $ splitPath repo1) 
                    ++ "-" ++ 
                    (filter (/= '/') $ last $ splitPath repo1)),
                    ((filter (/= '/') $ last $ init $ splitPath repo2) 
                    ++ "-" ++ 
                    (filter (/= '/') $ last $ splitPath repo2)),
                    lengthA, lengthB, overlap)
  else if "-tags" `intersect` (last $ init $ splitPath repo2) == "-tags" then
    convertToCSVLine ((takeBaseName repo1),
                    ((filter (/= '/') $ last $ init $ splitPath repo2)
                    ++ "-" ++ 
                    (filter (/= '/') $ last $ splitPath repo2)),
                    lengthA, lengthB, overlap)
  else
    convertToCSVLine ((takeBaseName repo1), (takeBaseName repo2),
                    lengthA, lengthB, overlap)

-- Compare repositories both original and after removal of whitespace and comments.
compareRepos :: String -> String -> Int -> String -> String -> IO ()
compareRepos repo1 repo2 flag hypothesis experiment = do
  dirlist1 <- traverseDir (const True) (\fs f -> pure (f : fs)) [] repo1
  dirlist2 <- traverseDir (const True) (\fs f -> pure (f : fs)) [] repo2
  let files1 = map (++ " ") dirlist1
  let files2 = map (++ " ") dirlist2

  let inter1 = map init $ filterFileType ".cpp " files1
  let inter2 = map init $ filterFileType ".cpp " files2

--  readFiles1 <- traverse (\x -> case try $ Txt.readFile x of
--                                  Left  except   -> print except
--                                  Right contents -> contents) inter1
  readFiles1 <- traverse readFileSafe inter1
  readFiles2 <- traverse readFileSafe inter2

  -- Remove C style comments and whitespace from files.
  let file1NoCWS = map Txt.pack $ map (concat . words) $ map (stripComments . Txt.unpack) readFiles1
  let file2NoCWS = map Txt.pack $ map (concat . words) $ map (stripComments . Txt.unpack) readFiles2

  --First iteration
  -- Get number of lines per file.
  let lenFiles1 = map (length . Txt.lines) readFiles1 -- n
  let lenFiles2 = map (length . Txt.lines) readFiles2 -- m

  let lenFilesNoCWS1 = map (length . Txt.lines) file1NoCWS -- n
  let lenFilesNoCWS2 = map (length . Txt.lines) file2NoCWS -- m

  -- To test on unmodified vs preprocessed use either  readFiles# or file#NoCWS.
  case flag of
    -- Test unmodified
    0 -> do
      -- First repo
      putStrLn $ "Comparing unmodified data " ++ takeBaseName repo1 ++ "-"
                                              ++ takeBaseName repo2 ++ " "
                                              ++ hypothesis         ++ " "
                                              ++ experiment
      let md5DigestOut1 = map (MD.md5 . BLU.fromString . Txt.unpack) readFiles1
      --let md5Text1 = map (toText . fromBytes . MD.md5DigestBytes) md5DigestOut1
      let md5Text1 = map (toText . fromBytes . MD.md5DigestBytes) md5DigestOut1
      let zipOfData1 = zip3 (map Txt.unpack md5Text1) (map show lenFiles1) inter1
      let csv1 = listify zipOfData1 []
      -- Second repo
      let md5DigestOut2 = map (MD.md5 . BLU.fromString . Txt.unpack) readFiles2
      let md5Text2 = map (toText . fromBytes . MD.md5DigestBytes) md5DigestOut2
      let zipOfData2 = zip3 (map Txt.unpack md5Text2) (map show lenFiles2) inter2
      let csv2 = listify zipOfData2 []

      let k1 = compressFiles $ sort csv1
      let k2 = compressFiles $ sort csv2 -- concat csv2
      let yy = compareCoinHashes k1 k2 (length k1) (length k2) ([], 0)
      let aa = snd yy
      let bb = fst yy

      let dat = convertToBasicCSVLine repo1 repo2 (length k1) (length k2) (length bb)
      --writeDataToFile (takeBaseName repo1) (takeBaseName repo2) dat hypothesis experiment
      writeBasicFile repo1 repo2 dat hypothesis experiment

    -- Test preprocessed
    1 -> do
      -- First repo preprocessed
      putStrLn $ "Comparing compressed data " ++ takeBaseName repo1 ++ "-"
                                              ++ takeBaseName repo2 ++ " "
                                              ++ hypothesis         ++ " "
                                              ++ experiment
      let md5DigestOut1 = map (MD.md5 . BLU.fromString . Txt.unpack) file1NoCWS
      let md5Text1 = map (toText . fromBytes . MD.md5DigestBytes) md5DigestOut1
      let zipOfData1 = zip3 (map Txt.unpack md5Text1) (map show lenFilesNoCWS1) inter1
      let csv1 = listify zipOfData1 []
      -- Second repo preprocessed
      let md5DigestOut2 = map (MD.md5 . BLU.fromString . Txt.unpack) file2NoCWS
      let md5Text2 = map (toText . fromBytes . MD.md5DigestBytes) md5DigestOut2
      let zipOfData2 = zip3 (map Txt.unpack md5Text2) (map show lenFilesNoCWS2) inter2
      let csv2 = listify zipOfData2 []

      let k1 = compressFiles $ sort csv1
      let k2 = compressFiles $ sort csv2
      let yy = compareCoinHashes k1 k2 (length k1) (length k2) ([], 0)
      let aa = snd yy
      let bb = fst yy

      let dat = convertToBasicCSVLine repo1 repo2 (length k1) (length k2) (length bb)
      writeBasicFile repo1 repo2 dat hypothesis experiment
--      if "-tags" `intersect` (last $ init $ splitPath repo2) == "-tags" 
--        && "-tags" `intersect` (last $ init $ splitPath repo1) == "-tags" then
--        -- if both repo 1 and 2 are tagged repos, build new name for writing
--        -- including the actual name of the coin.
--        writeDataToFile ((filter (/= '/') $ last $ init $ splitPath repo1) 
--                        ++ "-" ++ 
--                        (filter (/= '/') $ last $ splitPath repo1)) 
--                        ((filter (/= '/') $ last $ init $ splitPath repo2) 
--                        ++ "-" ++ 
--                        (filter (/= '/') $ last $ splitPath repo2))
--                        dat hypothesis experiment
--      else if "-tags" `intersect` (last $ init $ splitPath repo2) == "-tags" then
--        writeDataToFile (takeBaseName repo1) 
--                        ((filter (/= '/') $ last $ init $ splitPath repo2)
--                        ++ "-" ++ 
--                        (filter (/= '/') $ last $ splitPath repo2))
--                        dat hypothesis experiment
--      else
--        writeDataToFile (takeBaseName repo1) (takeBaseName repo2) dat 
--                        hypothesis experiment

    _ -> error "invalid flag value..."

  where
    listify :: [(a, a, a)] -> [[a]] -> [[a]]
    listify []        acc = acc
    listify ((x,y,z):xs) acc = listify xs (acc ++ [[x, y, z]])

foldRepos :: [(String, String)] -> Int -> String -> String -> IO ()
foldRepos    []  _ _ _                      = return ()
foldRepos (x:xs) flag hypothesis experiment = do
  --mapM_ (\y -> compareRepos x y flag hypothesis experiment) xs
  compareRepos (fst x) (snd x) flag hypothesis experiment
  foldRepos xs flag hypothesis experiment

compareAllBasicRepos :: [(String, String)] -> Int -> String -> IO ()
compareAllBasicRepos dat flag hyp = do
  --repos <- generateRepoList
  let hypothesis = "hypothesis_" ++ hyp
  let experiment = "basic" ++ show (flag + 1)
  --print hypothesis
  --print experiment
  foldRepos dat flag hypothesis experiment
