module Util where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text    as Txt
import qualified Data.Text.IO as Txt
import qualified Data.ByteString.Char8 as C

import qualified Data.List as L
--import           Data.List.Split
import           Data.List.Split (splitOn)
import           Data.Char
import           Data.String
import           System.IO
import           Control.Monad
import           Control.Applicative
import           Control.Exception
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

--import ParseTrees

coins_loc  = "/home/ghostbird/Hacking/cybersecurity/coins/"
--coins_loc = "/wheeler/scratch/khaskins/coins/"
data_final = "/home/ghostbird/Hacking/cybersecurity/fraud-cryptocurrency-codebases/data_final/"
--data_final = "/carc/scratch/projects/bridges2016099/data_final/"
commit_loc = "/home/ghostbird/Hacking/cybersecurity/fraud-cryptocurrency-codebases/commit_history/"
--commit_loc = "/wheeler/scratch/khaskins/fraud-cryptocurrency-codebases/commit_history/"
--commit_loc = "/carc/scratch/projects/bridges2016099/commit_history/"

-- Special State type used when filtering verions/tags.
type KeepState = ([Int], [(String, String, String)])

first :: (a, b, c) -> a
first (x,_,_) = x

second :: (a, b, c) -> b
second (_,x,_) = x

third :: (a, b, c) -> c
third (_,_,x) = x

g1st :: (a, b, c, d, e, f, g) -> a
g1st (x,_,_,_,_,_,_) = x

g2nd :: (a, b, c, d, e, f, g) -> b
g2nd (_,x,_,_,_,_,_) = x

g3rd :: (a, b, c, d, e, f, g) -> c
g3rd (_,_,x,_,_,_,_) = x

g4th :: (a, b, c, d, e, f, g) -> d
g4th (_,_,_,x,_,_,_) = x

g5th  :: (a, b, c, d, e, f, g) -> e
g5th (_,_,_,_,x,_,_) = x

get1st :: (a, b, c, d, e, f, g, h, i) -> a
get1st (x,_,_,_,_,_,_,_,_) = x

get2nd :: (a, b, c, d, e, f, g, h, i) -> b
get2nd (_,x,_,_,_,_,_,_,_) = x

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

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f x = iterate f x !! max 0 n

-- Parse all commit data files.
parseAllCommitFiles :: [String] -> [(String, String, String)] -> IO [(String, String, String)]
parseAllCommitFiles [] acc     = return acc
parseAllCommitFiles (f:fs) acc = do
  file <- readFileSafe f
  --print (head $ map Txt.unpack (Txt.lines file))
  let x = parseCommit (head $ map Txt.unpack (Txt.lines file)) (last $ map Txt.unpack (Txt.lines file))
  print (head $ map Txt.unpack (Txt.lines file))
  parseAllCommitFiles fs (acc ++ [x])
  where
    -- Parse single file.
    parseCommitsFile :: [String] -> String -> [(String, String, String)] -> [(String, String, String)]
    parseCommitsFile []     coin acc = acc
    parseCommitsFile (c:cs) coin acc = do
      let x = parseCommit c coin
      parseCommitsFile cs coin (acc ++ [x])
    -- Parse individual commit line from file.
--    parseCommit :: String -> (String, String)
--    parseCommit commit = let list = L.groupBy (\a b -> b /= ',') commit in
--                         (head list, head (tail list) ++ (head $ tail $ 
--                            L.groupBy (\a b -> b /= ' ') $ head (((tail list)))))
    parseCommit :: String -> String -> (String, String, String)
    parseCommit coin commit = let list = splitOn "," commit in
                         (coin, head list, nTimes 6 init $ tail $ head (tail list))

computeAllBasicHashScores :: [String] -> [(String, String, Float)] -> IO [(String, String, Float)]
computeAllBasicHashScores [] acc = return acc
computeAllBasicHashScores (f:fs) acc = do
  file <- readFileSafe f
  let x = parseBasicFile (map Txt.unpack (Txt.lines file)) []
  --print x
  computeAllBasicHashScores fs (acc ++ x)
  where
    parseBasicFile :: [String] -> [(String, String, Float)] -> [(String, String, Float)]
    parseBasicFile []     acc = acc
    parseBasicFile (x:xs) acc = do
      let score = computeScoreBasic x
      parseBasicFile xs (acc ++ [score])
    computeScoreBasic :: String -> (String, String, Float)
    computeScoreBasic line = let basic = splitOn "," line in
                                 ( head basic
                                 , head (tail basic)
                                 , if (first basic) < (second basic)
                                     then (same basic) / (first basic)
                                       else (same basic) / (second basic))
                                 --, ((((same basic) / (first  basic)) + ((same basic) / (second basic))) 
                                 -- / 2))
      where
        first  x = read (head (tail (tail x))) :: Float
        second x = read (head (tail (tail (tail x)))) :: Float
        same   x = read (head (tail (tail (tail (tail x))))) :: Float

computeAllParseTreeScores :: [String] -> [(String, String, Float)] -> IO [(String, String, Float)]
computeAllParseTreeScores [] acc = return acc
computeAllParseTreeScores (f:fs) acc = do
  file <- readFileSafe f
  let x = parseFile (map Txt.unpack (Txt.lines file)) [] 
  if length x /= 0 then  
    computeAllParseTreeScores fs (acc ++ [(first  $ head x
                                         , second $ head x
                                         , (foldr (+) 0 (map third x)) / (fromIntegral $ length x :: Float))])
    else 
    computeAllParseTreeScores fs (acc ++ [(first  $ head x
                                         , second $ head x
                                         , (foldr (+) 0 (map third x)) / (fromIntegral $ 1 :: Float))])
  where
    parseFile :: [String] -> [(String, String, Float)] -> [(String, String, Float)]
    parseFile []     acc = acc
    parseFile (x:xs) acc = do
      let score = computeScore x
      parseFile xs (acc ++ [score])
    computeScore :: String -> (String, String, Float)
    computeScore line = let tree = splitOn "," line in
                                 ( head tree
                                 , head (tail tree)
                                 , if (first tree) < (second tree) 
                                     then (same tree) / (first tree) else
                                       (same tree) / (second tree))
      where
        first  x = read (head (tail (tail (tail (tail x))))) :: Float
        second x = read (head (tail (tail (tail (tail (tail x)))))) :: Float
        same   x = read (head (tail (tail (tail (tail (tail (tail x))))))) :: Float

-- Aggregate all data
zipData :: [(String, String, Float)] 
        -> [(String, String, Float)] 
        -> [(String, String, Float)] 
        -> [(String, String, String)]
        -> [(String, String, String, String, String, String, Float, Float, Float)]
        -> [(String, String, String, String, String, String, Float, Float, Float)]
zipData []          []          []       []              acc = acc
zipData []          _           _        _               acc = acc
zipData _           []          _        _               acc = acc
zipData _           _           []       _               acc = acc
zipData _           _           _        []              acc = acc
zipData []          []          _        _               acc = acc
zipData _           []          []       _               acc = acc
zipData (b1:basic1) (b2:basic2) (a:asts) (commits) acc = do
  let c1 = foldr (\acc x -> if first b1 == first x then x else acc) ("","","") commits
  let c2 = foldr (\acc x -> if first b2 == first x then x else acc) ("","","") commits
  if (first b1) ==  (first b2) &&  (first b2) == (first a) &&
    (second b1) == (second b2) && (second b2) == (second a) 
                                          then zipData basic1 basic2 asts (commits) (acc ++ [(first b1
                                                                                          , second b1
                                                                                          , third c1
                                                                                          , third c2
                                                                                          , second c1
                                                                                          , second c2
                                                                                          , third b1
                                                                                          , third b2
                                                                                          , third a
                                                                                          )])
  else zipData (b1:basic1) (b2:basic2) (asts) (commits) acc
zipData _           _           _        _               acc = acc

generateScoreData :: IO ()
generateScoreData = do
  basic1     <- traverseDir (const True) (\fs f -> pure (f : fs)) [] 
      --"/wheeler/scratch/khaskins/fraud-cryptocurrency-codebases/data_final/hypothesis_1/basic1/"
      --"/carc/scratch/projects/bridges2016099/data_final/hypothesis_1/basic1/"
      "/home/ghostbird/Hacking/cybersecurity/fraud-cryptocurrency-codebases/data_final/hypothesis_1/basic1/"
  basic2     <- traverseDir (const True) (\fs f -> pure (f : fs)) [] 
      --"/wheeler/scratch/khaskins/fraud-cryptocurrency-codebases/data_final/hypothesis_1/basic2/"
      --"/carc/scratch/projects/bridges2016099/data_final/hypothesis_1/basic2/"
      "/home/ghostbird/Hacking/cybersecurity/fraud-cryptocurrency-codebases/data_final/hypothesis_1/basic2/"
  parseTrees <- traverseDir (const True) (\fs f -> pure (f : fs)) [] 
      --"/wheeler/scratch/khaskins/fraud-cryptocurrency-codebases/data_final/hypothesis_1/parse_trees/"
      --"/carc/scratch/projects/bridges2016099/data_final/hypothesis_1/parse_trees/"
      "/home/ghostbird/Hacking/cybersecurity/fraud-cryptocurrency-codebases/data_final/hypothesis_1/parse_trees"
  commitHist <- traverseDir (const True) (\fs f -> pure (f : fs)) [] 
      --"/wheeler/scratch/khaskins/fraud-cryptocurrency-codebases/commit_history/"
      --"/carc/scratch/projects/bridges2016099/commit_history/"
      "/home/ghostbird/Hacking/cybersecurity/fraud-cryptocurrency-codebases/commit_history"
  --print $ map takeBaseName basic1
  commits     <- parseAllCommitFiles commitHist []
  --print basic1
  basicHashes1 <- computeAllBasicHashScores basic1 []
  basicHashes2 <- computeAllBasicHashScores basic2 []
  parseTrees1  <- computeAllParseTreeScores parseTrees []
  let finalData = zipData basicHashes1 basicHashes2 parseTrees1 commits []
  let stringData = map convertToCSVLine9 finalData
  writeFinalDataToFile stringData
  --print commits
  --print basicHashes1
  --print basicHashes2
  --print parseTrees1
  --print finalData


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
-- Modified to only traverse one layer of a directory at a time.
traverseDir2 :: (FilePath -> Bool) -> (b -> FilePath -> IO b) -> b -> FilePath -> IO b
traverseDir2 validDir transition =
  let go state dirPath =
        do names <- listDirectory dirPath
           let paths = map (dirPath </>) names
           (dirPaths, filePaths) <- partitionM doesDirectoryExist paths
           foldM transition state (filter validDir dirPaths)
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


-- mkdir if directory does not exist, otherwise generates and filters out C++ files for directory.
generateFileList :: String -> IO [FilePath]
generateFileList repo = do
  (errc, out, err) <- readCreateProcessWithExitCode (shell ("mkdir " ++ repo)) []
  dirlist  <- traverseDir (const True) (\fs f -> pure (f : fs)) [] repo
  let dirs     = map (++ " ") dirlist
  let filtered = map init $ filterFileType ".cpp " dirs
  return filtered

-- Generates list of repos in ${DIR}/coins excluding "-tags" directories
generateRepoList :: IO [FilePath]
generateRepoList =
  traverseDir2 (not . L.isSuffixOf "-tags") (\fs f -> pure (f : fs)) [] coins_loc

generateRepoTags :: IO [FilePath]
generateRepoTags =
  traverseDir2 (L.isSuffixOf "-tags") (\fs f -> pure (f : fs)) [] coins_loc

-- Generates list of repos in ${DIR}/coins including only "-tags" directories
generateRepoTagList :: IO [FilePath]
generateRepoTagList = do
  dirlist <- traverseDir2 (L.isSuffixOf "-tags") (\fs f -> pure (f:fs)) [] coins_loc
  taglist <- mapM (traverseDir2 (const True) (\fs f -> pure (f:fs)) []) dirlist
  return $ concat taglist

generateRepoTagListBitcoin :: IO [FilePath]
generateRepoTagListBitcoin = do
  dirlist <- traverseDir2 (L.isSuffixOf "-tags") (\fs f -> if takeBaseName f == "bitcoin" then 
                                                              pure (f:fs) else pure fs) [] coins_loc
  taglist <- mapM (traverseDir2 (const True) (\fs f -> pure (f:fs)) []) dirlist
  return $ concat taglist

generateRepoTagListOld :: IO [FilePath]
generateRepoTagListOld = do
  dirlist <- traverseDir2 (L.isSuffixOf "-tags") (\fs f -> if takeBaseName f == "bitcoin"
                                                           || takeBaseName f == "litecoin"
                                                           || takeBaseName f == "rippled"
                                                           || takeBaseName f == "namecoin-core"
                                                           || takeBaseName f == "dogecoin"
                                                           then 
                                                              pure (f:fs) else pure fs) [] coins_loc
  taglist <- mapM (traverseDir2 (const True) (\fs f -> pure (f:fs)) []) dirlist
  return $ concat taglist

-- Split list by integer N into sublists.
splitListByN :: [a] -> Int -> [[a]]
splitListByN repos n = splitListHelper repos n []
  where
    splitListHelper :: [a] -> Int -> [[a]] -> [[a]]
    splitListHelper [] _ acc = acc
    splitListHelper xs n acc = splitListHelper (L.drop n xs) n (acc ++ [L.take n xs])

-- Takes list produced by generateRepoList/generateRepoTagList
-- Used for generating all pairs of repos to be compared and then
-- returning the requested subset for a given wheeler node.
--
-- ******** 20 is hardcoded for number of nodes in genTestSetHelper *************
--
generateTestSet :: [a] -> [a] -> Int -> Int -> [(a, a)]
generateTestSet set1 set2 subset jobs = genTestSetHelper set1 set2 subset jobs []
  where
    genTestSetHelper :: [a] -> [a] -> Int -> Int -> [(a, a)]
                                                 -> [(a, a)]
    genTestSetHelper     []      _ subset jobs acc = splitListByN acc (length acc `div` jobs) !! subset
    genTestSetHelper      _     [] subset jobs acc = splitListByN acc (length acc `div` jobs) !! subset
    genTestSetHelper (x:xs) (_:ys) subset jobs acc =
      genTestSetHelper xs ys subset jobs (acc ++ map (\y -> (x, y)) ys)

removeRepo :: String -> IO ()
removeRepo repo = do
  let str = "rm -rf " ++ coins_loc ++ repo
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

-- Clone a given repository based upon a list of three values:
-- ["BTC", "bitcoin", "https..."]
-- NEW :: Takes a tuple of ("Coin", "repo-link") and clones repo.
cloneRepo :: (String, String) -> IO ()
cloneRepo coin = do
  let str = "git clone --recursive " ++ snd coin ++ " " ++ coins_loc ++ fst coin
  (errc, out', err') <- readCreateProcessWithExitCode (shell str) []
  print $ fst coin

cloneRepos :: [(String, String)] -> IO ()
cloneRepos []     = do
  print ""
cloneRepos (x:xs) = do
  cloneRepo x
  -- print $ head $ tail x
  cloneRepos xs

gatherCommitHistory :: String -> IO ()
gatherCommitHistory repo = do
  let str1 = "cd " ++ repo ++ " ; "
  let str2 = "echo " ++ takeBaseName repo ++ " >> " ++ commit_loc 
                     ++ takeBaseName repo ++ ".csv && git log --pretty=\"%h, %cd\" >> " ++ commit_loc 
                     ++ takeBaseName repo ++ ".csv"
  (errc, out', err') <- readCreateProcessWithExitCode (shell (str1 ++ str2)) []
  putStrLn $ "Gathered commit history for " ++ takeBaseName repo

gatherAllCommitHistory :: [String] -> IO ()
gatherAllCommitHistory []     = putStrLn ""
gatherAllCommitHistory (x:xs) = do
  gatherCommitHistory x
  gatherAllCommitHistory xs


-- Gather commit data.
--performMultiple :: [a] -> (b -> IO ()) -> IO ()
performMultiple [] _ =
  print ""
performMultiple (x:xs) fun = do
  fun x
  performMultiple xs fun

getTags :: String -> IO [(String, String, String)]
getTags repo = do
  let str = "cd " ++ repo ++ " ; "
  let str2 = "git ls-remote --tags origin"
  (errc2, out2, err2) <- readCreateProcessWithExitCode (shell (str ++ str2)) []
  let complete = Txt.splitOn (Txt.pack "\n") (Txt.pack out2)
  let tuples = case length complete of
                 0 -> [(Txt.pack "", Txt.pack "1.0.0.0", repo)]
                 1 -> [(Txt.pack "", Txt.pack "1.0.0.0", repo)]
                 _ -> init $ map (\x -> (head $ Txt.splitOn (Txt.pack "\t") (head x), last x, repo))
                           $ map (Txt.splitOn (Txt.pack "/")) complete
  let unfiltered = map (\(x, y, z) -> (Txt.unpack x, Txt.unpack y, z)) tuples --filtered
  let filtered1  = map (\(a, b, c) -> (a, filter (/= 'v') $ b, c)) unfiltered
  let filtered2  = filter (\(i,j,k) -> foldr (\x acc -> acc && isDigit x) True $ filter (/= '.') j) filtered1
  return $ filter (\x -> not $ L.isSuffixOf "-tags" $ third x)  filtered2

--keepVersion :: String -> State String String -> Bool
keepVersions :: [(String, String, String)] -> State KeepState [(String, String, String)]
keepVersions [] = do
  (_, acc) <- get
  return acc
keepVersions (x:xs) = do
  (oldVersion, acc) <- get
  let newVersion = addZeroes $ toInts [] (wordsWhen (=='.') $ second x)
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
  let str  = "cp -r " ++ third dat ++ " " ++ new_path ++ second dat
  let str2 = " ; cd " ++ new_path ++ second dat ++ " ; git checkout " ++ first dat
  (errc, out, err) <- readCreateProcessWithExitCode (shell (str ++ str2)) []
  putStrLn $ "Copied and initialized " ++ third dat ++ " " ++ second dat


makeCopies :: [(String, String, String)] -> IO ()
makeCopies [] =
  putStrLn ""
makeCopies (x:xs) = do
  new_path <- initialCopy x
  makeCopy x new_path
  makeCopies xs

makeAllCopies :: [[(String, String, String)]] -> IO ()
makeAllCopies [[]] =
  putStrLn ""
makeAllCopies (x:xs) = do
  makeCopies x
  makeAllCopies xs
makeAllCopies _ =
  putStrLn ""

-- Get all tags and make all repo copies to <repo>-tag.
-- ** Function to call when generating copies of repos.
getAllTags :: IO ()
getAllTags = do
  repos <- generateRepoList
  tags_found <- generateRepoTags -- Generates list of repos with -tags suffix already
  let filtered = filterDirs repos tags_found
  tags  <- traverse getTags filtered
  let x = map filterMinorVersions tags
  -- Remove empty directories
  let out = filter (/=[]) x
  makeAllCopies out
  --print out

filterDirs :: [FilePath] -> [FilePath] -> [FilePath]
filterDirs repos tags = helper repos tags []
  where
    helper :: [FilePath] -> [FilePath] -> [FilePath] -> [FilePath]
    helper []     _  acc = acc
    helper (r:rs) ts acc = if (r ++ "-tags") `elem` ts then helper rs ts acc 
                                                       else helper rs ts (acc ++ [r])
                                                      

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

--listAllRepos :: IO [()]
--listAllRepos = do
--  input <- fmap Txt.lines $ Txt.readFile "misc/testset.csv"
--  let clean = fmap (\x -> fmap Txt.unpack x) $
--              fmap (\x -> (Txt.splitOn $ (Txt.pack ",") ) x) input
--  let tmp   = splitEvery 3 $ fmap (filter (/= '\n')
--            . filter (/= '\r')) $ concat clean
--  let repos = map takeFileName $ concat $ map (tail . tail) tmp
--  let pairs = buildRepos repos []
--  mapM (writeDataToFile "misc/repo-pairs.csv") pairs
  --print repos

buildRepos :: [String] -> [String]-> [String]
buildRepos    [] acc  = acc
buildRepos (x:xs) acc = do
  let m = map (\y -> convertToCSVTwo x y) xs
  --mapM (\y -> (print ("first repo: " ++ x ++ " second repo: " ++ y))) xs
  buildRepos xs (acc ++ m)



writeFinalDataToFile :: [String] -> IO ()
writeFinalDataToFile dat = do
  let fileNew =  data_final ++ "data_final.csv"
  let dataFinal = foldr (++) "" dat
  (errc, out, err) <- readCreateProcessWithExitCode (shell ("touch " ++ fileNew)) []
  h <- openFile fileNew AppendMode
  hPutStr h dataFinal
  hClose h


writeDataToFile :: String -> String -> String -> String -> String -> IO ()
writeDataToFile repo1 repo2 dat hypothesis experiment = do
  let fileNew =  data_final ++ hypothesis ++ "/" ++ experiment ++ "/" 
                            ++ (repo1 ++ "-" ++ repo2) ++ ".csv"
  (errc, out, err) <- readCreateProcessWithExitCode (shell ("touch " ++ fileNew)) []
  h <- openFile fileNew AppendMode
  hPutStr h dat
  hClose h

writeTreeToFile :: FilePath -> String -> String -> IO ()
writeTreeToFile file tree num = do
  let fileNew = "asts/" ++ takeBaseName file ++ num ++ ".ast"
  (errc, out, err) <- readCreateProcessWithExitCode (shell ("touch " ++ fileNew)) []
  h <- openFile fileNew WriteMode
  hPutStr h tree
  hClose h

readFileSafe :: FilePath -> IO Txt.Text
readFileSafe file = do
  h <- System.IO.openFile file System.IO.ReadMode
  System.IO.hSetEncoding h System.IO.utf8_bom
  text <- try (Txt.hGetContents h) :: IO (Either SomeException Txt.Text)
  case text of
    Left  exc -> return $ Txt.pack ""
    Right dat -> return dat
  

convertToCSVLine :: (String, String, Int, Int, Int) -> String
convertToCSVLine (a, b, c, d, e) = a ++ "," ++
                                   b ++ "," ++
                                   show c ++ "," ++
                                   show d ++ "," ++
                                   show e ++ "\n"

convertToCSVLine7 :: (String, String, String, String, Int, Int, Int) -> String
convertToCSVLine7 (a, b, c, d, e, f, g) = a ++ "," ++
                                          b ++ "," ++
                                          c ++ "," ++
                                          d ++ "," ++
                                     show e ++ "," ++
                                     show f ++ "," ++
                                     show g ++ "\n"

convertToCSVLine9 :: (String, String, String, String, String, String, Float, Float, Float) -> String
convertToCSVLine9 (a,b,c,d,e,f,g,h,i) = a ++ "," ++
                                        b ++ "," ++
                                        c ++ "," ++
                                        d ++ "," ++
                                        e ++ "," ++
                                        f ++ "," ++
                                   show g ++ "," ++
                                   show h ++ "," ++
                                   show i ++ "\n"

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
