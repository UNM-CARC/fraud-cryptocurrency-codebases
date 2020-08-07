module Lib where


--import           Data.Word8
--import           Data.Digest.Pure.MD5
--data DirTree a = LeafHash a | Node (DirTree a) (DirTree a)
--type HashList = [(Word8, MD5Digest)]

import qualified Data.Map.Strict as Map
import qualified Data.List as L
import           Data.Map.Strict (Map)

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
