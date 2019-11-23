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
import LangColours (loadJSON)

handleMaybe :: Maybe a -> a
handleMaybe (Just a) = a

{-A program that creates a social graph of a given user. 
The User and all their followers are represented as circles with connecting lines representing followers.
 The size and colour of the circle represent the total size of their repos and thei most used language respectively. 
Implemented 100% in Haskell for the challenge.-}

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

        colourJson <- loadJSON

        let followerNames = getFollowerNames userProfile
        followerProfiles <- Prelude.mapM (\x -> nameToProfile x (Just hashset)) followerNames

        let pic  = Graphics.draw (followerProfiles ++ [userProfile]) colourJson

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