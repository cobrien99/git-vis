module Main where

import GitFollowers
import GitRepos

main :: IO ()
main = do
          name <- getLine
          publicity <- getLine
          --result <- githubFollowers input
          result <- getUsersRepos name publicity
          print result