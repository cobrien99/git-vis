module Main where

import GitFollowers
import GitRepos
import Prompt

main :: IO ()
main = do
        name <- prompt "enter a GitHub username: " 
        publicity <- prompt "See all users repos or only owned repos: " 
        --result <- githubFollowers input
        result <- getUsersRepos name publicity
        putStrLn result