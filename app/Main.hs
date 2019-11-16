module Main where

import Git

main :: IO ()
main = do input <- getLine
          followers <- githubFollowers input
          putStrLn followers