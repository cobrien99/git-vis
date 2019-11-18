module Main where

import GitFollowers
import GitRepos
import Prompt
import Data.Vector as V(map)
import System.Environment (getEnv)
import LoadEnv
import User
import GitHub.Auth as A
import qualified Data.ByteString.Char8 as C

handleMaybe :: Maybe a -> a
handleMaybe (Just a) = a

main :: IO ()
main = do
        loadEnv
        token <- getEnv "TOKEN"
        name <- prompt "enter a GitHub username: " 
        publicity <- prompt "See all users repos or only owned repos: "
        --result <- githubFollowers input
        repos <- getUsersRepos name publicity (Just (A.OAuth (C.pack token)))
        followers <- githubFollowers name (Just (A.OAuth (C.pack token)))
        let profile = makeUser name (handleMaybe repos) (handleMaybe followers)
        print profile
