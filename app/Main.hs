module Main where

import GitFollowers
import GitRepos
import Prompt
import Data.Vector as V(map)

handleMaybe :: Maybe a -> a
handleMaybe (Just a) = a

main :: IO ()
main = do
        name <- prompt "enter a GitHub username: " 
        publicity <- prompt "See all users repos or only owned repos: " 
        --result <- githubFollowers input
        result <- getUsersRepos name publicity
        putStrLn (concat (V.map show (handleMaybe result)))
