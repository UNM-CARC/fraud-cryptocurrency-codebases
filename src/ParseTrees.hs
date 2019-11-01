module ParseTrees where

import qualified Data.GraphViz.Commands.IO as DOT
import qualified System.Directory as DIR
--import qualified Data.Graph.Inductive.Graph as G
import qualified Data.GraphViz.Types as T
import qualified Data.Text    as Txt
import qualified Data.Text.IO as Txt
import qualified Data.Map.Strict as Map
--import qualified Data.Text.Internal.Lazy as TxtG
import           Data.List
import           Data.Ord (comparing)
import           Data.GraphViz.Types.Generalised
import           System.Process
import           System.FilePath.Posix
import           Control.Monad
import           Language.C.Clang
import           Language.C.Clang.Cursor
import           Data.Map.Strict (Map)
--import           Language.C.Clang

--import DotParser
--import DotPretty

subStrings :: [a] -> [[a]]
subStrings s =
  let intChars = length s
  in [ take n $ drop i s
     | i <- [0 .. intChars - 1] 
     , n <- [1 .. intChars - i] ]
 
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

treeToString :: FilePath -> IO String
treeToString file = do
  idx <- createIndex
  tu  <- parseTranslationUnit idx file ["-I/usr/local/include"]
  let root = translationUnitCursor tu
      str  = serialize root
      --filt = filter (not . (`elem` "abcdefghijklmnopqrstuvwxyz")) str
      --hsh  = foldl (\acc y -> Map.lookup ) "" str
  return str

serialize :: Cursor -> String
serialize root = '(' : (let x = Map.lookup (show $ cursorKind root) kindHash in
                            case x of Just o  -> o
                                      Nothing -> "00") 
                    ++ (foldl (\acc y -> acc ++ serialize y) "" (cursorChildren root))
                    ++ ")"

compareTrees :: FilePath -> FilePath -> IO ()
compareTrees file1 file2 = do
  x <- treeToString file1 
  y <- treeToString file2
  let out = longestCommon x y
  print out

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
kindHash :: Map String String
kindHash = Map.fromList $ zip kind $ take 189 [z | x <- alph, y <- alph, let z = x:[y]]
  where 
    alph = "abcdefghijklmnopqrstuvwxyz"
    kind = 
        [ "UnexposedDecl",
          "StructDecl ",
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
