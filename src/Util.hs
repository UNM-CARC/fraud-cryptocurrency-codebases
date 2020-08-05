module Util where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text    as Txt
import qualified Data.Text.IO as Txt
import qualified Data.ByteString.Char8 as C

import qualified Data.List as L
--import           Data.List.Split
import           Data.Char
import           Data.String
import           System.IO
import           Control.Monad
import           Control.Applicative
import           System.Process
import           Data.Digest.Pure.MD5
--import           Control.Monad (foldM)
import           System.Directory (doesDirectoryExist, listDirectory, getDirectoryContents)
import           System.FilePath ((</>), FilePath)
import           System.FilePath.Posix
import           Control.Monad.Extra (partitionM)
import           Control.Monad.State
--import           Text.Regex.TDFA
--import           Text.Regex.PCRE
import           System.Directory.Tree (
                   AnchoredDirTree(..), DirTree(..),
                   filterDir, readDirectoryWith
                   )
import           System.FilePath (takeExtension)

import Lib
--import ParseTrees

first :: (a, b, c) -> a
first (x,_,_) = x

second :: (a, b, c) -> b
second (_,x,_) = x

third :: (a, b, c) -> c
third (_,_,x) = x

g1st :: (a, b, c, d, e) -> a
g1st (x,_,_,_,_) = x

g2nd :: (a, b, c, d, e) -> b
g2nd (_,x,_,_,_) = x

g3rd :: (a, b, c, d, e) -> c
g3rd (_,_,x,_,_) = x

g4th :: (a, b, c, d, e) -> d
g4th (_,_,_,x,_) = x

g5th  :: (a, b, c, d, e) -> e
g5th (_,_,_,_,x) = x

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
filterFileType s xs = filter (\x -> if L.isInfixOf s x then True else False) xs
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

-- https://stackoverflow.com/questions/51712083/recursively-search-directories-for-all-files-matching-name-criteria-in-haskell
traverseDir2 :: (FilePath -> Bool) -> (b -> FilePath -> IO b) -> b -> FilePath -> IO b
traverseDir2 validDir transition =
  let go state dirPath =
        do names <- listDirectory dirPath
           let paths = map (dirPath </>) names
           (dirPaths, filePaths) <- partitionM doesDirectoryExist paths
           state' <- foldM transition state dirPaths -- process current dir
           return state'
           in go


walkDir :: FilePath -> IO [FilePath]
walkDir r = contents >>= fmap concat . traverse helper
    where contents = fmap (r </>) . filter ((&&) . (/=) "." <*> (/=) "..") <$> getDirectoryContents r
          helper x = do e <- doesDirectoryExist x
                        if e then walkDir x else return [x]


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

-- Use for names.csv
-- [(repo, git-link)]
filterRepoLinks :: [[String]] -> [(String, String)]
filterRepoLinks repos =
  let x = select (\z -> length z /= 0) (map last repos) repos in
      map (\y -> ((takeBaseName . last) y, last y)) x


generateFileList :: String -> IO [FilePath]
generateFileList repo = do
  let str = "mkdir /wheeler/scratch/khaskins/coins/" ++ repo
  (errc, out, err) <- readCreateProcessWithExitCode (shell str) []
  dirlist  <- traverseDir (\_ -> True) (\fs f -> pure (f : fs)) []
                          ("/wheeler/scratch/khaskins/coins/" ++ repo)
  let dirs     = map (\x -> x ++ " ") dirlist
  let filtered = map init $ filterFileType ".cpp " dirs
  return filtered

generateRepoList :: IO [FilePath]
generateRepoList = do
  dirlist  <- traverseDir2 (const True) (\fs f -> pure (f : fs)) [] "/wheeler/scratch/khaskins/coins/"
  --dirlist  <- traverseDir2 (const True) (\fs f -> pure (f : fs)) [] 
  --            "/home/ghostbird/Hacking/cybersecurity/coins/"
  --dirlist <- walkDir "/wheeler/scratch/khaskins/coins/"
  return dirlist

removeRepo :: String -> IO ()
removeRepo repo = do
  let str = "rm -rf /wheeler/scratch/khaskins/coins/" ++ repo
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
-- NEW :: Takes a tuple of ("Coin", "repo-link") and clones repo.
cloneRepo :: (String, String) -> IO ()
cloneRepo coin = do
  let str = "git clone --recursive " ++ snd coin ++ " /wheeler/scratch/khaskins/coins/" ++ fst coin
  (errc, out', err') <- readCreateProcessWithExitCode (shell str) []
  print $ fst coin

cloneRepos :: [(String, String)] -> IO ()
cloneRepos []     = do
  print ""
cloneRepos (x:xs) = do
  cloneRepo x
  -- print $ head $ tail x
  cloneRepos xs

getTags :: String -> IO [(String, String, String)]
getTags repo = do
  let str = "cd " ++ repo ++ " ; "
  --let str = "cd /wheeler/scratch/khaskins/coins/" ++ repo ++ " ; "
  --let str = "cd /home/ghostbird/Hacking/cybersecurity/coins/" ++ repo ++ " ; "
  -- ls-remote
  --let str2 = "git for-each-ref --sort=taggerdate --format '%(tag) %(taggerdate:raw)' refs/tags"
  --let str2 = "git show-ref --tags"
  --let str2 = "git ls-remote --tags origin | grep -v -e rc -e {} -e alpha -e dev -e build -e poc -e test -e release -e Tester -e noversion"
  --let str2 = "git ls-remote --tags origin | grep -vE 'debug|ref|rc|{}|alpha|dev|build|poc|test|release|Tester|noversion|+'"
  let str2 = "git ls-remote --tags origin"
  --let str2 = "git log --oneline --decorate --tags --no-walk"
  -- | grep -v {} | grep -v alpha | grep -v dev | grep -v build | grep -v poc | grep -v test | grep -v release | grep -v Tester | grep -v noversion"
  (errc2, out2, err2) <- readCreateProcessWithExitCode (shell (str ++ str2)) []
  let complete = Txt.splitOn (Txt.pack "\n") (Txt.pack out2)
  let tuples = case length complete of
                 0 -> [(Txt.pack "", Txt.pack "1.0.0.0", repo)] 
                 1 -> [(Txt.pack "", Txt.pack "1.0.0.0", repo)] 
                 _ -> init $ map (\x -> (head $ Txt.splitOn (Txt.pack "\t") (head x), last x, repo)) $ map (Txt.splitOn (Txt.pack "/")) complete
  --let filtered = map (\(x, y) -> (head $ Txt.splitOn (Txt.pack "\\") x, y)) tuples
  --return $ map Txt.unpack
  let unfiltered = map (\(x, y, z) -> (Txt.unpack x, Txt.unpack y, z)) tuples --filtered
  --print unfiltered
  let filtered1  = map (\(a, b, c) -> (a, filter (/= 'v') $ b, c)) unfiltered
  --let tmp        = filter (\= '.') $ second 
  --let tmp        = map second filtered1
  let filtered2  = filter (\(i,j,k) -> foldr (\x acc -> acc && isDigit x) True $ filter (/= '.') j) filtered1
  --let filtered1  = map (\x -> filter (/= 'v') $ second x) unfiltered
  return $ filter (\x -> not $ L.isSuffixOf "-tags" $ third x)  filtered2

type KeepState = ([Int], [(String, String, String)])

--keepVersion :: String -> State String String -> Bool
keepVersions :: [(String, String, String)] -> State KeepState [(String, String, String)]
keepVersions [] = do
  (_, acc) <- get
  return acc
keepVersions (x:xs) = do
  (oldVersion, acc) <- get
  --let last    = filter (/= 'v') lastVersion
  --let currentVersion = wordsWhen (=='.') $ second x
  --let verOld = map (foldl (\(i, j) k -> ((read k :: Int) * j, j * 10)) (0, 1)) $ wordsWhen (=='.') lastVersion
  --let verOld = toInts $ wordsWhen (=='.') lastVersion
  let newVersion = addZeroes $ toInts [] (wordsWhen (=='.') $ second x)
  -- Remove all but numeric digits and convert to integer values.
  --let las  = fst $ foldl (\(y, z) x -> ((read x :: Int) * z, z * 10)) (0, 1) [filter isDigit lastVersion]
  --let curr = fst $ foldl (\(y, z) x -> ((read x :: Int) * z, z * 10)) (0, 1) [filter isDigit current]
  let version = determineVersion newVersion oldVersion x

  if length (first version) /= 0 then
    put (newVersion, acc ++ [version])
  else
    put (newVersion, acc)
  keepVersions xs
  where
    toInts :: [Int] -> [String] -> [Int]
    toInts acc []     = acc
    toInts acc (y:ys) = toInts (acc ++ [fst $ foldl (\(i, j) k -> ((read k :: Int) * j, j * 10)) (0, 1) [y]]) ys
 
    addZeroes :: [Int] -> [Int]
    addZeroes xs = case length xs of
                     0 -> xs ++ [0,0,0,0]
                     1 -> xs ++ [0,0,0]
                     2 -> xs ++ [0,0]
                     3 -> xs ++ [0]
                     _ -> xs
    
    determineVersion :: [Int] -> [Int] -> (String, String, String) -> (String, String, String)
    determineVersion xs ys version
      | head xs == head ys && head (tail xs) == head (tail ys) 
                           && head (tail $ tail xs) < head (tail $ tail ys) = ("","","")
      | head xs == head ys && head (tail xs) == head (tail ys) 
                           && head (tail $ tail xs) == head (tail $ tail ys) 
                           && head (tail $ tail xs) < head (tail $ tail ys) = ("","","")
      | head xs < head ys || head (tail xs) < head (tail ys) = version
      | otherwise = ("","","")

-- (SHA1, Version, Directory)
-- Called once per coin
filterMinorVersions :: [(String, String, String)] -> [(String, String, String)]
filterMinorVersions y =
  --filter (\(_, y, _) -> evalState (keepVersion y) "0.0.0")
  reverse $ evalState (keepVersions $ reverse y) ([99,99,99,99], [])

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

--getDate :: (String, String, String) -> (String, String, String, String)
--getDate
--
--getDates :: [(String, String, String)] -> [(String, String, String, String)]
--getDates []     =
--  putStrLn ""
--getDates (x:xs) = do
--  getDate x
--  getDates xs


-- Used to initially make new top-level directory.
initialCopy :: (String, String, String) -> IO String
initialCopy dat = do
  --let str = "mkdir /wheeler/scratch/khaskins/" ++ third data ++ "-tags"
  let str = third dat ++ "-tags/"
  --let str = "/home/ghostbird/Hacking/cybersecurity/coins/" ++ third dat ++ "-tags/"
  (errc, out, err) <- readCreateProcessWithExitCode (shell ("mkdir " ++ str)) []
  return str


makeCopy :: (String, String, String) -> String -> IO ()
makeCopy dat new_path = do
  --let str = "cp -r /wheeler/scratch/khaskins/" ++ third dat ++ " " ++ new_path ++ snd dat
  --let str  = "cp -r /home/ghostbird/Hacking/cybersecurity/coins/" ++ third dat ++ " " ++ new_path ++ second dat
  let str  = "cp -r " ++ third dat ++ " " ++ new_path ++ second dat
  let str2 = " ; cd " ++ new_path ++ second dat ++ " ; git checkout " ++ first dat
  (errc, out, err) <- readCreateProcessWithExitCode (shell (str ++ str2)) []
  putStrLn $ "Copied and initialized " ++ third dat ++ " " ++ second dat


makeCopies :: [(String, String, String)] -> IO ()
makeCopies []   = do
  print ""
makeCopies (x:xs) = do
  new_path <- initialCopy x
  makeCopy x new_path
  makeCopies xs

makeAllCopies :: [[(String, String, String)]] -> IO ()
makeAllCopies [[]] = do
  print ""
makeAllCopies (x:xs) = do
  makeCopies x
  makeAllCopies xs

-- Get all tags and make all repo copies to <repo>-tag.
-- ** Function to call when generating copies of repos.
getAllTags :: IO ()
getAllTags = do
  repos <- generateRepoList
  tags  <- traverse getTags repos
  let x = map filterMinorVersions tags
  -- Remove empty directories
  let out = filter (/=[]) x
  print out

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
