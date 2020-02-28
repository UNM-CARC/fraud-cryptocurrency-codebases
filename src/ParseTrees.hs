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


--lcstr xs ys = maximumBy (compare `on` length) . concat $ [f xs' ys | xs' <- tails xs] ++ [f xs ys' | ys' <- drop 1 $ tails ys]
--  where f xs ys = scanl g [] $ zip xs ys
--        g z (x, y) = if x == y then z ++ [x] else []

-- Rosetta Code: Longest Common Substring
-- So far seems to be too inefficient
--subStrings :: [a] -> [[a]]
--subStrings s =
--  let intChars = length s
--  in [ take n $ drop i s
--     | i <- [0..intChars - 1]
--     , n <- [1..intChars - i] ]
--
--longestCommon :: Eq a => [a] -> [a] -> [a]
--longestCommon a b =
--  maximumBy (comparing length) (subStrings a `intersect` subStrings b)

longestCommon :: Eq a => [a] -> [a] -> [a]
longestCommon a b =
  L.maximumBy (comparing length) $
  (uncurry L.intersect . pair) $ [tail . L.inits <=< L.tails] <*> [a, b]

pair :: [a] -> (a, a)
pair [x, y] = (x, y)

--stringArr :: Int -> Int -> Array Int Int
--stringArr x y =
--  array (0, x * y) [(x, z) | x <- [0..(x * y)], ]
--
--longestCommon :: String -> String -> Int
--longestCommon a b
--  where lcSuff =

parseCPP :: FilePath -> IO String
parseCPP file = do
    idx <- createIndex
    tu  <- parseTranslationUnit idx file ["-I/usr/local/include"]
    let root = translationUnitCursor tu
        --children = cursorChildren root
        --children = L.foldr (\y x -> if (L.foldr (\yy xx -> if (show $ cursorKind y)
        --                                             == yy then False else xx) True filterOut)
        --                           then y : x else x) [] (cursorDescendants root)
        children = serialize2 root
        --functionDecls = filter (\c -> cursorKind c == FunctionDecl) children
    --forM_ children (print . cursorSpelling)
    --print root
    --print $ map cursorKind children
    --mapM_ print $ map cursorKind children
    return children

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
      --filt = filter (not . (`elem` "abcdefghijklmnopqrstuvwxyz")) str
      --hsh  = foldl (\acc y -> Map.lookup ) "" str
      return str

keep :: [String]
keep =      [ "FunctionDecl"
            , "ParmDecl"
            , "StructDecl"
            , "UnexposedDecl"
            , "UnexposedExpr"
            , "IntegerLiteral"
            ]

filterOut :: [String]
filterOut = [ "TypedefDecl"
            , "TypeRef"
            , "TypeAliasDecl"
            , "UsingDeclaration"
            , "UsingDirective"
            , "TemplateRef"
            , "TemplateTypeParameter"
            , "FunctionTemplate"
            , "ClassTemplate"
            , "TemplateTemplateParameter"
            --, "FirstExpr"
            --, "LastExpr"
            --, "FunctionDecl"
            --, "IntegerLiteral"
            --, "ParmDecl"
            , "UsingDeclaration"
            , "FirstAttr"
            , "OverloadedDeclRef"
            , "Namespace"
            , "NamespaceRef"
            , "DeclRefExpr"
            --, "ParenExpr"
            --, "CStyleCastExpr"
            --, "UnexposedDecl"
            --, "UnexposedExpr"
            , "FirstRef"
            , "LastRef"
            , "MemberRefExpr"
            , "EnumConstantDecl"
            , "NamespaceAlias"
            , "ModuleImportDecl"
            , "CXXBaseSpecifier"
            , "FieldDecl"
            ]

serialize :: Cursor -> String
serialize root = '(' : (let x = Map.lookup (show $ cursorKind root) kindHash in
                           case x of Just o  -> o     -- Replace CursorKind with 2 letter string.
                                     Nothing -> "zz")
                    ++ (L.foldl (\acc y -> if (L.foldr (\yy xx -> if (show $ cursorKind y)
                                                                == yy then False
                                                                        else xx) True filterOut)
                                            then acc ++ serialize y
                                              else acc) "" (cursorChildren root))
                    ++ ")"

serialize2 :: Cursor -> String
serialize2 root = '(' : (show $ cursorKind root)
                    ++ (L.foldl (\acc y -> if (L.foldr (\yy xx -> if (show $ cursorKind y)
                                                                == yy then False
                                                                        else xx) True filterOut)
                                            then acc ++ serialize2 y
                                              else acc) "" (cursorChildren root))
                    ++ ")"

compareTrees :: FilePath -> FilePath -> IO (String, String, Int, Int, Int) -- IO ()
compareTrees file1 file2 = do
  x <- treeToString file1
  y <- treeToString file2
  --writeTreeToFile file1 x "1"
  --writeTreeToFile file2 y "2"
  let out  = longestCommonSubstring $ x : [y]
  let test = (takeFileName file1, takeFileName file2, length x, length y, length out)
  return test
  --print $ "Size of tree x: " ++ (show $ length x)
  --print $ "Size of tree y: " ++ (show $ length y)
  --print $ "Size of subtree: " ++ (show $ length out)
  --print out

buildTrees :: FilePath -> FilePath -> IO ()
buildTrees file1 file2 = do
  x <- evaluate $ treeToString file1
  y <- evaluate $ treeToString file2
  m <- x
  n <- y
  writeTreeToFile file1 m "1"
  writeTreeToFile file2 n "2"
  --let out  = longestCommonSubstring $ x : [y]
  --let test = (takeFileName file1, takeFileName file2, length x, length y, length out)
  --return test

g1st :: (String, String, Int, Int, Int) -> String
g1st (x,_,_,_,_) = x

g3rd :: (a, b, c, d, e) -> c
g3rd (_,_,x,_,_) = x

g4th :: (a, b, c, d, e) -> d
g4th (_,_,_,x,_) = x

g5th  :: (a, b, c, d, e) -> e
g5th (_,_,_,_,x) = x

compareAllParseTrees :: [FilePath] -> [FilePath] -> IO [(String, String, Int, Int, Int)]
compareAllParseTrees xs ys = sequence $ helper xs ys []
  where
    helper :: [FilePath] -> [FilePath] -> [IO (String, String, Int, Int, Int)]
                                       -> [IO (String, String, Int, Int, Int)]
    helper []     _  acc = acc
    helper (f:fs) ms acc = let
      --fun <- L.foldr (\y a -> a ++ [(compareTrees f y)]) [] ms
      fun = map (\y -> if (takeFileName f) == (takeFileName y) then 
                         [(compareTrees f y)] 
                       else ([return ("NULL","NULL",0,0,0)])) ms in
      --let test = fmap (\n -> let (h,i,j,k,l) = n in (h,i,j,k,l)) fun
      --let xx = L.foldr (\r m -> let (a, b, c, d, e) = r in (a,b,c,d,e) : m) [] test
        helper fs ms (acc ++ (concat fun))

--compareAllParseTrees :: [FilePath] -> [FilePath] -> IO [(String, String, Int, Int, Int)]
--compareAllParseTrees xs ys = sequence $ helper xs ys []
--  where
--    helper :: [FilePath] -> [FilePath] -> [IO (String, String, Int, Int, Int)]
--                                       -> [IO (String, String, Int, Int, Int)]
--    helper []     _  acc = acc
--    helper (f:fs) ms acc = let
--      --fun <- L.foldr (\y a -> a ++ [(compareTrees f y)]) [] ms
--      fun = map (\y -> [(compareTrees f y)]) ms in
--      --let test = fmap (\n -> let (h,i,j,k,l) = n in (h,i,j,k,l)) fun
--      --let xx = L.foldr (\r m -> let (a, b, c, d, e) = r in (a,b,c,d,e) : m) [] test
--      helper fs ms (acc ++ (concat fun))

--compareAllParseTrees (f:fs) ys acc = compareAllParseTrees fs ys (acc ++ fun)
--  where
--    fun = L.foldr (\y a -> a ++ [(compareTrees f y)]) [] ys
    --xx f2 = do
    --  m <- compareTrees f f2
    --  let yy = m
    --  yy

buildAllParseTrees :: [FilePath] -> [FilePath] -> IO [()]
buildAllParseTrees xs ys = sequence $ helper xs ys []
  where
    helper :: [FilePath] -> [FilePath] -> [IO ()]
                                       -> [IO ()]
    helper []     _  acc = acc
    helper (f:fs) ms acc = let
      fun = map (\y -> [(buildTrees f y)]) ms in
      helper fs ms (acc ++ (concat fun))

compareAllRepos :: IO ()
compareAllRepos = do
  input <- fmap Txt.lines $ Txt.readFile "misc/testset.csv"
  let clean = fmap (\x -> fmap Txt.unpack x) $
              fmap (\x -> (Txt.splitOn $ (Txt.pack ",") ) x) input
  let tmp   = splitEvery 3 $ fmap (L.filter (/= '\n')
            . L.filter (/= '\r')) $ concat clean
  let repos = map takeFileName $ concat $ map (tail . tail) tmp
  mapRepos repos
  --print repos

mapRepos :: [String] -> IO ()
mapRepos    []  = return ()
mapRepos (x:xs) = do
  mapM (\y -> compareParseTreesRepos x y) xs
  --mapM (\y -> (print ("first repo: " ++ x ++ " second repo: " ++ y))) xs
  mapRepos xs

compareParseTreesRepos :: String -> String -> IO () -- [(String, String, Int, Int, Int)]
compareParseTreesRepos repo1 repo2 = do
  dirlist1  <- traverseDir (\_ -> True) (\fs f -> pure (f : fs)) [] ("/wheeler/scratch/khaskins/" ++ repo1)
  dirlist2  <- traverseDir (\_ -> True) (\fs f -> pure (f : fs)) [] ("/wheeler/scratch/khaskins/" ++ repo2)
  let dirs1  = map (\x -> x ++ " ") dirlist1
  let dirs2  = map (\x -> x ++ " ") dirlist2
  let inter1 = map init $ filterFileType ".cpp " dirs1
  let inter2 = map init $ filterFileType ".cpp " dirs2
  --print inter1
  --print inter2
  let subset1 = L.take 200 inter1
  let subset2 = L.take 200 inter2
  --print subset1
  out    <- compareAllParseTrees subset1 subset2
  let test = L.foldl (\a x -> if g1st x /= "NULL" then a ++ [x] else a) [] out
  let out2 = L.foldl (\y a -> a ++ "\n" ++ y) "" (map convertToCSVLine test)
  writeDataToFile (repo1 ++ "-" ++ repo2) out2
  --out
  --print inter1

buildParseTreesRepos :: String -> String -> IO [()]
buildParseTreesRepos repo1 repo2 = do
  dirlist1  <- traverseDir (\_ -> True) (\fs f -> pure (f : fs)) [] ("/tmp/" ++ repo1)
  dirlist2  <- traverseDir (\_ -> True) (\fs f -> pure (f : fs)) [] ("/tmp/" ++ repo2)
  let dirs1  = map (\x -> x ++ " ") dirlist1
  let dirs2  = map (\x -> x ++ " ") dirlist2
  let inter1 = map init $ filterFileType ".cpp " dirs1
  let inter2 = map init $ filterFileType ".cpp " dirs2
  --print inter1
  --print inter2
  let out    = buildAllParseTrees inter1 inter2
  out

generateAST :: String -> String -> IO ()
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

kindHash :: Map String String
kindHash = Map.fromList $ zip kind $ L.take 189 [z | x <- alph, y <- alph, let z = x:[y]]
  where
    alph = "abcdefghijklmnopqrstuvwxyz"
    kind =
        [ "UnexposedDecl",
          "StructDecl",
          "UnionDecl",
          "ClassDecl",
          "EnumDecl",
          "FieldDecl",
          "EnumConstantDecl",
          "FunctionDecl",
          "VarDecl",
          "ParmDecl",
          "ObjCInterfaceDecl",
          "ObjCCategoryDecl",
          "ObjCProtocolDecl",
          "ObjCPropertyDecl",
          "ObjCIvarDecl",
          "ObjCInstanceMethodDecl",
          "ObjCClassMethodDecl",
          "ObjCImplementationDecl",
          "ObjCCategoryImplDecl",
          "TypedefDecl",
          "CXXMethod",
          "Namespace",
          "LinkageSpec",
          "Constructor",
          "Destructor",
          "ConversionFunction",
          "TemplateTypeParameter",
          "NonTypeTemplateParameter",
          "TemplateTemplateParameter",
          "FunctionTemplate",
          "ClassTemplate",
          "ClassTemplatePartialSpecialization",
          "NamespaceAlias",
          "UsingDirective",
          "UsingDeclaration",
          "TypeAliasDecl",
          "ObjCSynthesizeDecl",
          "ObjCDynamicDecl",
          "CXXAccessSpecifier",
          "FirstDecl",
          "LastDecl",
          "FirstRef",
          "ObjCSuperClassRef",
          "ObjCProtocolRef",
          "ObjCClassRef",
          "TypeRef",
          "CXXBaseSpecifier",
          "TemplateRef",
          "NamespaceRef",
          "MemberRef",
          "LabelRef",
          "OverloadedDeclRef",
          "VariableRef",
          "LastRef",
          "FirstInvalid",
          "InvalidFile",
          "NoDeclFound",
          "NotImplemented",
          "InvalidCode",
          "LastInvalid",
          "FirstExpr",
          "UnexposedExpr",
          "DeclRefExpr",
          "MemberRefExpr",
          "CallExpr",
          "ObjCMessageExpr",
          "BlockExpr",
          "IntegerLiteral",
          "FloatingLiteral",
          "ImaginaryLiteral",
          "StringLiteral",
          "CharacterLiteral",
          "ParenExpr",
          "UnaryOperator",
          "ArraySubscriptExpr",
          "BinaryOperator",
          "CompoundAssignOperator",
          "ConditionalOperator",
          "CStyleCastExpr",
          "CompoundLiteralExpr",
          "InitListExpr",
          "AddrLabelExpr",
          "StmtExpr",
          "GenericSelectionExpr",
          "GNUNullExpr",
          "CXXStaticCastExpr",
          "CXXDynamicCastExpr",
          "CXXReinterpretCastExpr",
          "CXXConstCastExpr",
          "CXXFunctionalCastExpr",
          "CXXTypeidExpr",
          "CXXBoolLiteralExpr",
          "CXXNullPtrLiteralExpr",
          "CXXThisExpr",
          "CXXThrowExpr",
          "CXXNewExpr",
          "CXXDeleteExpr",
          "UnaryExpr",
          "ObjCStringLiteral",
          "ObjCEncodeExpr",
          "ObjCSelectorExpr",
          "ObjCProtocolExpr",
          "ObjCBridgedCastExpr",
          "PackExpansionExpr",
          "SizeOfPackExpr",
          "LambdaExpr",
          "ObjCBoolLiteralExpr",
          "ObjCSelfExpr",
          "LastExpr",
          "FirstStmt",
          "UnexposedStmt",
          "LabelStmt",
          "CompoundStmt",
          "CaseStmt",
          "DefaultStmt",
          "IfStmt",
          "SwitchStmt",
          "WhileStmt",
          "DoStmt",
          "ForStmt",
          "GotoStmt",
          "IndirectGotoStmt",
          "ContinueStmt",
          "BreakStmt",
          "ReturnStmt",
          "GCCAsmStmt",
          "AsmStmt",
          "ObjCAtTryStmt",
          "ObjCAtCatchStmt",
          "ObjCAtFinallyStmt",
          "ObjCAtThrowStmt",
          "ObjCAtSynchronizedStmt",
          "ObjCAutoreleasePoolStmt",
          "ObjCForCollectionStmt",
          "CXXCatchStmt",
          "CXXTryStmt",
          "CXXForRangeStmt",
          "SEHTryStmt",
          "SEHExceptStmt",
          "SEHFinallyStmt",
          "MSAsmStmt",
          "NullStmt",
          "DeclStmt",
          "OMPParallelDirective",
          "OMPSimdDirective",
          "OMPForDirective",
          "OMPSectionsDirective",
          "OMPSectionDirective",
          "OMPSingleDirective",
          "OMPParallelForDirective",
          "OMPParallelSectionsDirective",
          "OMPTaskDirective",
          "OMPMasterDirective",
          "OMPCriticalDirective",
          "OMPTaskyieldDirective",
          "OMPBarrierDirective",
          "OMPTaskwaitDirective",
          "OMPFlushDirective",
          "SEHLeaveStmt",
          "LastStmt",
          "TranslationUnit",
          "FirstAttr",
          "UnexposedAttr",
          "IBActionAttr",
          "IBOutletAttr",
          "IBOutletCollectionAttr",
          "CXXFinalAttr",
          "CXXOverrideAttr",
          "AnnotateAttr",
          "AsmLabelAttr",
          "PackedAttr",
          "PureAttr",
          "ConstAttr",
          "NoDuplicateAttr",
          "CUDAConstantAttr",
          "CUDADeviceAttr",
          "CUDAGlobalAttr",
          "CUDAHostAttr",
          "LastAttr",
          "PreprocessingDirective",
          "MacroDefinition",
          "MacroExpansion",
          "MacroInstantiation",
          "InclusionDirective",
          "FirstPreprocessing",
          "LastPreprocessing",
          "ModuleImportDecl",
          "FirstExtraDecl",
          "LastExtraDecl"
        ]
