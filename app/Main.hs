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
import Graphics
import Graphics.Gloss as G
import Data.HashSet as H

handleMaybe :: Maybe a -> a
handleMaybe (Just a) = a

main :: IO ()
main = do
        loadEnv
        token <- getEnv "TOKEN"
        name <- prompt "Enter a GitHub username: " 
        publicity <- prompt "See all users repos or only owned repos: "
        removeForks <- prompt "Ignore forked repos: "

        let nameToProfile = \x ->  makeProfile x publicity removeForks token

        userProfile <- nameToProfile name Nothing

        let hashset = hashFollowers (getFollowerNames userProfile)

        let followerNames = getFollowerNames userProfile
        followerProfiles <- Prelude.mapM (\x -> nameToProfile x (Just hashset)) followerNames

        let pic  = Graphics.draw followerProfiles

        Graphics.show pic


        print userProfile


makeProfile :: String -> String -> String -> String -> Maybe (HashSet String) -> IO User
makeProfile name publicity removeForks token hashset = do
        let getRepos = \x -> getUsersRepos x publicity removeForks (Just (A.OAuth (C.pack token)))
        let getFollowers = \x -> githubFollowers x (Just (A.OAuth (C.pack token)))
        repos <- getRepos name
        followers <- getFollowers name

        let filteredFollowers = filterFollowersByHashset (handleMaybe followers) hashset
        --filter to only include users that follow the main user

        let userProfile = makeUser name (handleMaybe repos) filteredFollowers
        return userProfile