module ParseC where

import Language.C
import Language.C.System.GCC
import System.Environment 
import Control.Monad
import Language.C.Clang
import Language.C.Clang.Cursor

parseCPP file = do
    idx <- createIndex
    tu <- parseTranslationUnit idx file ["-I/usr/local/include"]
    let root = translationUnitCursor tu
        children = cursorChildren root
        --functionDecls = filter (\c -> cursorKind c == FunctionDecl) children
    --forM_ children (print . cursorSpelling)
    print children

-- Parser for C exclusively
process :: String -> IO ()
process file = do 
  putStr file 
  stream <- readInputStream file 
  putStr (take (20 - length file) $ repeat ' ') 
  --either print (const $ putStrLn "Pass") (parseC stream nopos)
  print $ parseC stream nopos

--main :: IO ()
--main = do 
--  files <- getArgs 
--  mapM_ process files
  --print files
