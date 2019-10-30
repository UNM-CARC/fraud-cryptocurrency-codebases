module ParseTrees where

import qualified Data.GraphViz.Commands.IO as DOT
import qualified System.Directory as DIR
--import qualified Data.Graph.Inductive.Graph as G
import qualified Data.GraphViz.Types as T
import qualified Data.Text    as Txt
import qualified Data.Text.IO as Txt
--import qualified Data.Text.Internal.Lazy as Txt
import           Data.GraphViz.Types.Generalised
import           System.Process
import           System.FilePath.Posix
import           Control.Monad
import           Language.C.Clang
import           Language.C.Clang.Cursor
--import           Language.C.Clang

--import DotParser
--import DotPretty


parseCPP file = do
    idx <- createIndex
    tu <- parseTranslationUnit idx file ["-I/usr/local/include"]
    let root = translationUnitCursor tu
        --children = cursorChildren root
        children = cursorDescendants root
        --functionDecls = filter (\c -> cursorKind c == FunctionDecl) children
    --forM_ children (print . cursorSpelling)
    --print root
    print $ map cursorKind children


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
