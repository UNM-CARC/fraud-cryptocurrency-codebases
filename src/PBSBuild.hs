module PBSBuild where

import           Data.List
import           Data.Char
import           System.IO
import           Control.Monad
import           Control.Applicative
import           System.Process

import Util

generateCommitDirectory :: IO ()
generateCommitDirectory = do
  let dir = "mkdir commit_history"
  (errc, out, err) <- readCreateProcessWithExitCode(shell dir) []
  putStrLn "Generated commit history directory"

generateOutputDirectories :: IO ()
generateOutputDirectories = do
  let dir1 = "mkdir data_final ;"
  let dir2 = "mkdir data_final/hypothesis_1 ;"
  let dir3 = "mkdir data_final/hypothesis_2 ;"
  let dir4 = "mkdir data_final/hypothesis_3 ;"
  let dir5 = "mkdir data_final/hypothesis_1/basic1 ;"
  let dir6 = "mkdir data_final/hypothesis_1/basic2 ;"
  let dir7 = "mkdir data_final/hypothesis_1/parse_trees ;"
  let dir8 = "mkdir data_final/hypothesis_2/basic1 ;"
  let dir9 = "mkdir data_final/hypothesis_2/basic2 ;"
  let dir10 = "mkdir data_final/hypothesis_2/parse_trees ;"
  let dir11 = "mkdir data_final/hypothesis_3/basic1 ;"
  let dir12 = "mkdir data_final/hypothesis_3/basic2 ;"
  let dir13 = "mkdir data_final/hypothesis_3/parse_trees ;"
  
  (errc, out, err) <- readCreateProcessWithExitCode (shell (dir1  ++
                                                            dir2  ++
                                                            dir3  ++
                                                            dir4  ++
                                                            dir5  ++
                                                            dir6  ++
                                                            dir7  ++
                                                            dir8  ++
                                                            dir9  ++
                                                            dir10 ++
                                                            dir11 ++
                                                            dir12 ++
                                                            dir13
                                                           )) []
  putStrLn "Generated final data directory structure"

cleanOutputDirectories :: IO ()
cleanOutputDirectories = do
  let dir1 = "rm -rf data_final "
  let dir2 = "data_final/hypothesis_1 "
  let dir3 = "data_final/hypothesis_2 "
  let dir4 = "data_final/hypothesis_3 "
  let dir5 = "data_final/hypothesis_1/basic1 "
  let dir6 = "data_final/hypothesis_1/basic2 "
  let dir7 = "data_final/hypothesis_1/parse_trees "
  let dir8 = "data_final/hypothesis_2/basic1 "
  let dir9 = "data_final/hypothesis_2/basic2 "
  let dir10 = "data_final/hypothesis_2/parse_trees "
  let dir11 = "data_final/hypothesis_3/basic1 "
  let dir12 = "data_final/hypothesis_3/basic2 "
  let dir13 = "data_final/hypothesis_3/parse_trees "
  
  (errc, out, err) <- readCreateProcessWithExitCode (shell (dir1  ++
                                                            dir2  ++
                                                            dir3  ++
                                                            dir4  ++
                                                            dir5  ++
                                                            dir6  ++
                                                            dir7  ++
                                                            dir8  ++
                                                            dir9  ++
                                                            dir10 ++
                                                            dir11 ++
                                                            dir12 ++
                                                            dir13
                                                           )) []
  putStrLn "Cleaned up final data directories"


