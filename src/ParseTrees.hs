module ParseTrees where

--import qualified Data.GraphViz.Commands.IO as DOT
import qualified System.Directory as DIR
-- import qualified Data.GraphViz.Types as T
import qualified Data.Text    as Txt
import qualified Data.Text.IO as Txt
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.List as L
import           Data.Set hiding (map, drop)
import           Data.Function
import           Data.Ord (comparing)
import           System.Process
import           System.IO
import           System.FilePath.Posix
import           Control.Monad
import           Language.C.Clang
import           Language.C.Clang.Cursor
import           Data.Map.Strict (Map)
import           Control.Exception

import Util
import Lib

-- "https://codereview.stackexchange.com/questions/164889/finding-the
-- -longest-common-substring-of-multiple-strings-in-haskell"
-- find all the substrings of length n
ngrams :: Int -> String -> [String]
ngrams n s | n > length s = []
           | otherwise    = L.take n s : ngrams n (drop 1 s)

-- find the longest common substring of multiple strings
longestCommonSubstring :: [String] -> String
longestCommonSubstring xs = go 0 $ length (head xs) + 1
 where
  -- find a substring of a given length n that is common to all strings
  commonSubstrings n = foldr1 S.intersection (map (S.fromList . ngrams n) xs)
  go l r
    -- if the binary search ended, pick one common substring
    | r - l == 1 = S.findMin $ commonSubstrings l
    | otherwise
    = if S.null $ commonSubstrings m
      then go l m    -- the length is too big, try a smaller one
      else go m r    -- try longer length to find longer substring
   where
    m = (l + r) `div` 2    -- the middle point

serialize :: Cursor -> String
serialize root = '(' : (let x = Map.lookup (show $ cursorKind root) kindHash in
                           case x of Just o  -> o     -- Replace CursorKind with 2 letter string.
                                     Nothing -> "zz")
                    ++ L.foldl (\acc y -> if (L.foldr (\yy xx -> if show (cursorKind y)
                                                                == yy then False
                                                                        else xx) True Lib.filterOut)
                                            then acc ++ serialize y
                                              else acc) "" (cursorChildren root)
                    ++ ")"

treeToString :: FilePath -> IO String
treeToString file = do
  idx <- createIndex
  tu  <- try (parseTranslationUnit idx file ["-I/usr/local/include"])
                                            :: IO (Either SomeException TranslationUnit)
  case tu of
    Left ex -> return $ "Caught: " ++ show ex
    Right trans -> do
      let root = translationUnitCursor trans
          str  = serialize root
      return str

-- Takes two files, parses and converts them into ast strings, compares them and
-- outputs a tuple containing the name of each file, their length, and the combined
-- longest common substring length.
compareTrees :: FilePath -> FilePath -> IO (String, String, Int, Int, Int) -- IO ()
compareTrees file1 file2 = do
  x <- treeToString file1
  y <- treeToString file2
  let longest  = longestCommonSubstring $ x : [y]
  let out      = (takeFileName file1, takeFileName file2, length x, length y, length longest)
  return out
  --print $ "Size of tree x: " ++ (show $ length x)
  --print $ "Size of tree y: " ++ (show $ length y)
  --print $ "Size of subtree: " ++ (show $ length out)
  --print out

-- Takes two lists of files to compare and produces a list of tuples
compareAllParseTrees :: [FilePath] -> [FilePath] -> IO [(String, String, Int, Int, Int)]
compareAllParseTrees xs ys = sequence $ helper xs ys []

  where
    helper :: [FilePath] -> [FilePath] -> [IO (String, String, Int, Int, Int)]
                                       -> [IO (String, String, Int, Int, Int)]
    helper []     _  acc = acc
    helper (f:fs) ms acc = let
      fun = map (\y -> if takeFileName f == takeFileName y then
                         [compareTrees f y]
                       else [return ("NULL","NULL",0,0,0)]) ms in
      --let test = fmap (\n -> let (h,i,j,k,l) = n in (h,i,j,k,l)) fun
      --let xx = L.foldr (\r m -> let (a, b, c, d, e) = r in (a,b,c,d,e) : m) [] test
        helper fs ms (acc ++ concat fun)

compareParseTreeRepos :: String -> String -> String -> String -> IO ()
compareParseTreeRepos repo1 repo2 hypothesis experiment = do
  dirlist1  <- generateFileList repo1 --traverseDir (const True) (\fs f -> pure (f : fs)) [] repo1
  dirlist2  <- generateFileList repo2 --traverseDir (const True) (\fs f -> pure (f : fs)) [] repo2
  --let dirs1   = map (++ " ") dirlist1
  --let dirs2   = map (++ " ") dirlist2
  --let inter1  = map init $ filterFileType ".cpp " dirs1
  --let inter2  = map init $ filterFileType ".cpp " dirs2
  let subset1 = L.take 200 dirlist1
  let subset2 = L.take 200 dirlist2
  out    <- compareAllParseTrees subset1 subset2
  let test = L.foldl (\a x -> if g1st x /= "NULL" then a ++ [x] else a) [] out
  --let out2 = L.foldl (\y a -> a ++ "\n" ++ y) "" (map convertToCSVLine test)
  let out2 = concatMap convertToCSVLine test
  writeDataToFile (takeBaseName repo1) (takeBaseName repo2) out2 hypothesis experiment


--mapRepos :: [String] -> String -> String -> IO ()
mapRepos :: [(String, String)] -> String -> String -> IO ()
mapRepos    []  _ _                   = return ()
mapRepos (x:xs) hypothesis experiment = do
  --mapM_ (\y -> compareParseTreeRepos x y hypothesis experiment) xs
  --mapM_ (\y -> print ("first repo: " ++ x ++ " second repo: " ++ y)) xs
  compareParseTreeRepos (fst x) (snd x) hypothesis experiment
  --uncurry compareParseTreeRepos x x hypothesis experiment
  mapRepos xs hypothesis experiment

-- **********************************************
-- Entry point for comparison of all parse trees.
-- **********************************************
compareAllParseTreeRepos :: [(String, String)] -> String -> IO ()
compareAllParseTreeRepos dat hyp = do
--  input <- fmap Txt.lines $ Txt.readFile "misc/testset.csv"
--  let clean = fmap (\x -> fmap Txt.unpack x) $
--              fmap (\x -> (Txt.splitOn $ (Txt.pack ",") ) x) input
--  let tmp   = splitEvery 3 $ fmap (L.filter (/= '\n')
--            . L.filter (/= '\r')) $ concat clean
--  let repos = map takeFileName $ concat $ map (tail . tail) tmp
  --repos <- generateRepoList
  let hypothesis = "hypothesis_" ++ hyp
  let experiment = "parse_trees"
  mapRepos dat hypothesis experiment
