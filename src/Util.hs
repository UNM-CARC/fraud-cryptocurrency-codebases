module Util where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text    as Txt
import qualified Data.Text.IO as Txt
import qualified Data.ByteString.Char8 as C

import           Data.List
import           Data.Char
import           Data.String
import           System.IO
import           Control.Monad
import           Control.Applicative
import           System.Process
import           Data.Digest.Pure.MD5
--import           Control.Monad (foldM)
import           System.Directory (doesDirectoryExist, listDirectory) 
import           System.FilePath ((</>), FilePath)
import           System.FilePath.Posix
import           Control.Monad.Extra (partitionM)
--import           Text.Regex.TDFA
--import           Text.Regex.PCRE
import           System.Directory.Tree (
                   AnchoredDirTree(..), DirTree(..),
                   filterDir, readDirectoryWith
                   )
import           System.FilePath (takeExtension)

import Lib
--import ParseTrees

-- C style comment removal :: https://stackoverflow.com/questions/7904805/haskell-program-to-remove-comments
stripComments :: String -> String
stripComments [] = []
stripComments ('/':'/':xs) = inComment xs 
stripComments ('/':'*':xs) = inMultiComment xs
stripComments ('\"':xs) = '\"' : inString xs
stripComments (x:xs) = x : stripComments xs

inComment :: String -> String
inComment [] = []
inComment ('\n':xs) = stripComments xs
inComment (_:xs) = inComment xs

inMultiComment :: String -> String
inMultiComment [] = []
inMultiComment ('*':'/':xs) = stripComments xs
inMultiComment (_:xs) = inMultiComment xs

inString :: String -> String
inString [] = []
inString ('\"':xs) = '\"' : stripComments xs
inString (x:xs) = x : inString xs

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
--  (acc ++ (foldr (\y a -> if head y ==
--                            head x then (y : fst a, snd a + 1)
--                                     else (fst a, snd a)) ([], 0.0) ys))
  where
    fun = (foldr (\y a -> if head y ==
                             head x then (y : fst a, snd a + 1)
                                    else (fst a, snd a)) ([], 0) ys)

filterFileType :: String -> [String] -> [String]
filterFileType s xs = filter (\x -> if isInfixOf s x then True else False) xs
--filterFileType s xs = filter (\x -> if isInfixOf s x then False else True) xs -- Old

-- Remove duplicate files
compressFiles :: [[String]] -> [[String]]
compressFiles files = foldr (\b a -> if a == []
  then b:a else if (head b) == (head $ head a)
                  then (head a ++ [last b]) : tail a
                    else b : a) [] files

-- https://stackoverflow.com/questions/51712083/recursively-search-directories-for-all-files-matching-name-criteria-in-haskell
traverseDir :: (FilePath -> Bool) -> (b -> FilePath -> IO b) -> b -> FilePath -> IO b
traverseDir validDir transition =
  let go state dirPath =
        do names <- listDirectory dirPath
           let paths = map (dirPath </>) names
           (dirPaths, filePaths) <- partitionM doesDirectoryExist paths
           state' <- foldM transition state filePaths -- process current dir
           foldM go state' (filter validDir dirPaths) -- process subdirs
           in go

readDataCSV :: String -> IO [[String]]
readDataCSV dat = do
  input <- fmap Txt.lines $ Txt.readFile ("data/" ++ dat)
  let clean = fmap (\x -> fmap Txt.unpack x) $
              fmap (\x -> (Txt.splitOn $ (Txt.pack ",") ) x) input
  let tmp   = splitEvery 5 $ fmap (filter (/= '\n')
            . filter (/= '\r')) $ concat clean
  return tmp

select :: (t1 -> Bool) -> [t1] -> [t] -> [t]
select p xs ys = [y | (x,y) <- zip xs ys, p x]

-- Use for repos.csv
-- [(repo, git link)]
filterSelected :: [[String]] -> [(String, String)]
filterSelected repos = 
  let x = select (=="1") (map (head . tail . tail . tail . tail) repos) repos in 
    map (\y -> ((head . tail . tail) y, (head . tail . tail . tail) y)) x

generateFileList :: String -> IO [FilePath]
generateFileList repo = do
  let str = "mkdir /wheeler/scratch/khaskins/" ++ repo
  (errc, out, err) <- readCreateProcessWithExitCode (shell str) []
  dirlist  <- traverseDir (\_ -> True) (\fs f -> pure (f : fs)) [] 
                          ("/wheeler/scratch/khaskins/" ++ repo)
  let dirs     = map (\x -> x ++ " ") dirlist
  let filtered = map init $ filterFileType ".cpp " dirs
  return filtered

removeRepo :: String -> IO ()
removeRepo repo = do
  let str = "rm -rf /wheeler/scratch/khaskins/" ++ repo
  (errc, out, err) <- readCreateProcessWithExitCode (shell str) []
  print $ "Removed " ++ repo

pruneRepo :: String -> IO ()
pruneRepo repo = do
  fileList <- generateFileList repo
  let len = length fileList
  if len > 0 
    then
      print $ "Keeping " ++ repo
    else
      removeRepo repo

pruneRepos :: [(String, String)] -> IO ()
pruneRepos []     = do
  print ""
pruneRepos (x:xs) = do
  pruneRepo $ fst x
  pruneRepos xs
  
--transferToTuple :: [[String]] -> [(String, String)]
--transferToTuple repos =
--  map (\x -> (head x, (head . tail . tail) x)) repos

-- Clone a given repository based upon a list of three values:
-- ["BTC", "bitcoin", "https..."]
-- NEW :: Takes a tuple of ("Coin", "repo") and clones repo.
cloneRepo :: (String, String) -> IO ()
cloneRepo coin = do
  let str = "git clone --recursive " ++ snd coin ++ " /wheeler/scratch/khaskins/" ++ fst coin
  (errc, out', err') <- readCreateProcessWithExitCode (shell str) []
  print $ fst coin

cloneRepos :: [(String, String)] -> IO ()
cloneRepos []     = do
  print ""
cloneRepos (x:xs) = do
  cloneRepo x
  -- print $ head $ tail x
  cloneRepos xs

getTags :: String -> IO () --[(String, String)]
getTags coin = do
  let str = "cd /wheeler/scratch/khaskins/" ++ coin ++ " ; "
  --let str = "cd /home/ghostbird/Hacking/cybersecurity/" ++ coin ++ " ; "
  let str2 = "git ls-remote --tags origin | grep -v rc | grep -v {} | grep -v alpha | grep -v dev | grep -v build | grep -v poc | grep -v test | grep -v release | grep -v Tester | grep -v noversion"
  (errc2, out2, err2) <- readCreateProcessWithExitCode (shell (str ++ str2)) []
  let complete = Txt.splitOn (Txt.pack "\n") (Txt.pack out2)
  let tuples   = init $ map (\x -> (head x, last x)) $ map (Txt.splitOn (Txt.pack "/")) complete
  let filtered = map (\(x, y) -> (head $ Txt.splitOn (Txt.pack "\\") x, y)) tuples
  --return $ map Txt.unpack 
  print filtered


--cloneRepositoryByYears :: (String, String) -> IO ()
--cloneRepositoryByYears coin = do
--  taglist <- getTags $ fst coin


--allFiles :: String -> IO (DirTree FilePath)
--allFiles dir = do
--    _:/tree <- readDirectoryWith return dir
--    let x = filterDir prd tree
--    --print x
--    return x
--  where prd (Dir ('.':_) _) = False
--        prd _ = True

-- Split a list of a into a list of lists every n values.
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
  where (as,bs) = splitAt n xs

listAllRepos :: IO [()]
listAllRepos = do
  input <- fmap Txt.lines $ Txt.readFile "misc/testset.csv"
  let clean = fmap (\x -> fmap Txt.unpack x) $
              fmap (\x -> (Txt.splitOn $ (Txt.pack ",") ) x) input
  let tmp   = splitEvery 3 $ fmap (filter (/= '\n')
            . filter (/= '\r')) $ concat clean
  let repos = map takeFileName $ concat $ map (tail . tail) tmp
  let pairs = buildRepos repos []
  mapM (writeDataToFile "misc/repo-pairs.csv") pairs
  --print repos
  
buildRepos :: [String] -> [String]-> [String]
buildRepos    [] acc  = acc
buildRepos (x:xs) acc = do
  let m = map (\y -> convertToCSVTwo x y) xs
  --mapM (\y -> (print ("first repo: " ++ x ++ " second repo: " ++ y))) xs
  buildRepos xs (acc ++ m)
  


writeDataToFile :: FilePath -> String -> IO ()
writeDataToFile file dat = do
  let fileNew = "data/" ++ (takeBaseName file) ++ ".csv"
  (errc, out, err) <- readCreateProcessWithExitCode (shell ("touch " ++ fileNew)) []
  h <- openFile fileNew AppendMode
  hPutStr h dat
  hClose h

writeTreeToFile :: FilePath -> String -> String -> IO ()
writeTreeToFile file tree num = do
  let fileNew = "asts/" ++ (takeBaseName file) ++ num ++ ".ast"
  (errc, out, err) <- readCreateProcessWithExitCode (shell ("touch " ++ fileNew)) []
  h <- openFile fileNew WriteMode
  hPutStr h tree
  hClose h

convertToCSVLine :: (String, String, Int, Int, Int) -> String
convertToCSVLine (a, b, c, d, e) = a ++ "," ++
                                   b ++ "," ++
                                   show c ++ "," ++
                                   show d ++ "," ++
                                   show e

convertToCSVTwo :: String -> String -> String
convertToCSVTwo a b = a ++ "," ++ b ++ "\n"

lessThan :: [String] -> Float
lessThan (_:_:x:y:[z]) = do
  let xx = read x :: Float
  let yy = read y :: Float
  let zz = read z :: Float
  if yy < xx then
       zz / yy
  else
       zz / xx

greaterThan :: [String] -> Float
greaterThan (_:_:x:y:[z]) = do
  let xx = read x :: Float
  let yy = read y :: Float
  let zz = read z :: Float
  if yy > xx then
       zz / yy
  else
       zz / xx

average :: [String] -> Float
average (_:_:x:y:[z]) = do
  let xx = read x :: Float
  let yy = read y :: Float
  let zz = read z :: Float
  zz / ((xx + yy) / 2)

  

--  Process individual data files. For now compute average of AST sizes.
--  [[String]] list of lists where each sublist contains the names of the two files being compared
--  along with the respective size of each AST and the length of the combined longest substring 
--  AST.
processDataFile :: [[String]] -> [String]
processDataFile ys = do 
  funcLess ys []
  funcGrea ys []
  funcAvg  ys []
  where
    funcLess [] acc     = map show acc
    funcLess (x:xs) acc =
      funcLess xs (acc ++ [lessThan x])
    funcGrea [] acc     = map show acc
    funcGrea (x:xs) acc =
      funcGrea xs (acc ++ [greaterThan x])
    funcAvg [] acc     = map show acc
    funcAvg (x:xs) acc =
      funcAvg xs (acc ++ [average x])


--analyzeAllData :: IO ()
--analyzeAllData = do
--  dat <- traverseDir (\_ -> True) (\fs f -> pure (f : fs)) [] ("data2/")
--  read_files <- traverse readDataCSV dat
--  let processed = map processDataFile read_files
--  writeDataToFile "cumulative_data" processed
