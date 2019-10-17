module ParseTrees where

import qualified Data.GraphViz.Commands.IO as DOT
import qualified System.Directory as DIR
import qualified Data.Graph.Inductive.Graph as G
import           System.Process
import           System.FilePath.Posix


generateAST repo file = do
  DIR.createDirectoryIfMissing True ("/tmp/AST/" ++ repo ++ "/")
  let command = prefix ++ suffix
  (errc, out, err) <- readCreateProcessWithExitCode (shell command) []
  print command
  where
    prefix = "clang -cc1 -ast-dump " ++ file
    suffix = " > /tmp/AST/" ++ repo ++ "/" ++ (dropExtension $ takeFileName file) ++ ".ast"

readAST repo file = do
  let x = DOT.readDotFile ("/tmp/AST/" ++ repo ++ "/" ++ (dropExtension $ takeFileName file) ++ ".ast") -- :: IO (G.Node)
  let y = x
  DOT.putDot y
