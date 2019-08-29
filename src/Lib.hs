module Lib where

data DirTree a = LeafHash a | Node (DirTree a) (DirTree a)
