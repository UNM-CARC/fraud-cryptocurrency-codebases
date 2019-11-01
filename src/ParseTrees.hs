module ParseTrees where

import qualified Data.GraphViz.Commands.IO as DOT
import qualified System.Directory as DIR
--import qualified Data.Graph.Inductive.Graph as G
import qualified Data.GraphViz.Types as T
import qualified Data.Text    as Txt
import qualified Data.Text.IO as Txt
--import qualified Data.Text.Internal.Lazy as Txt
import           Data.List
import           Data.Ord (comparing)
import           Data.GraphViz.Types.Generalised
import           System.Process
import           System.FilePath.Posix
import           Control.Monad
import           Language.C.Clang
import           Language.C.Clang.Cursor
--import           Language.C.Clang

--import DotParser
--import DotPretty

--subStrings :: [a] -> [[a]]
--subStrings s =
--  let intChars = length s
--  in [ take n $ drop i s
--     | i <- [0 .. intChars - 1] 
--     , n <- [1 .. intChars - i] ]
-- 
--longestCommon :: Eq a => [a] -> [a] -> [a]
--longestCommon a b =
--  maximumBy (comparing length) (subStrings a `intersect` subStrings b)

longestCommon :: Eq a => [a] -> [a] -> [a]
longestCommon a b =
  maximumBy (comparing length) $
  (uncurry intersect . pair) $ [tail . inits <=< tails] <*> [a, b]
 
pair :: [a] -> (a, a)
pair [x, y] = (x, y)

--parseCPP :: FilePath -> [Cursor]
parseCPP file = do
    idx <- createIndex
    tu  <- parseTranslationUnit idx file ["-I/usr/local/include"]
    let root = translationUnitCursor tu
        --children = cursorChildren root
        children = cursorDescendants root
        --functionDecls = filter (\c -> cursorKind c == FunctionDecl) children
    --forM_ children (print . cursorSpelling)
    --print root
    --print $ map cursorKind children
    mapM_ print $ map cursorKind children

treeToString file = do
  idx <- createIndex
  tu  <- parseTranslationUnit idx file ["-I/usr/local/include"]
  let root = translationUnitCursor tu
      str  = serialize root
  print str

serialize :: Cursor -> String
serialize root = '(' : (show $ cursorKind root) 
                    ++ (foldl (\acc y -> acc ++ serialize y) "" (cursorChildren root))
                    ++ ")"

{-
serializeCPP file = do
    idx <- createIndex
    tu  <- parseTranslationUnit idx file ["-I/usr/local/include"]
    let root = translationUnitCursor tu
        children = cursorChildren root
        
        --functionDecls = filter (\c -> cursorKind c == FunctionDecl) children
    --forM_ children (print . cursorSpelling)
    --print root
    --print $ map cursorKind children
    mapM_ print $ map cursorKind children
-}

generateAST repo file = do
  DIR.createDirectoryIfMissing True ("/tmp/AST/" ++ repo ++ "/")
  let command = prefix ++ suffix
  (errc, out, err) <- readCreateProcessWithExitCode (shell command) []
  print command
  where
    prefix = "clang -cc1 -ast-dump " ++ file
    suffix = " > /tmp/AST/" ++ repo ++ "/" ++ (dropExtension $ takeFileName file) ++ ".ast"

--parseCSource file = do
--  idx       <- createIndex
--  transUnit <- parseTranslationUnit idx file [""]
--  print transUnit -- translationUnitCursor transUnit

--readAST repo file = do
--  h <- Txt.readFile ("/tmp/AST/" ++ repo ++ "/" ++ (dropExtension $ takeFileName file) ++ ".ast")
--  let y = Txt.unpack h
--  let x = parseDot y
--  prettyPrintDot x
--  DOT.putDot x
--  let x = DOT.readDotFile ("/tmp/AST/" ++ repo ++ "/" ++ (dropExtension $ takeFileName file) ++ ".ast") -- :: DotGraph a
--  let y = x
--  DOT.putDot y
