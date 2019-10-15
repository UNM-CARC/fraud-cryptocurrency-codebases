module ParseTrees where

import qualified Language.C.Parser as P

import qualified Language.C.Data.Position as PO

generateAST stream file = P.parseC stream (PO.position 0 file 0 0)
