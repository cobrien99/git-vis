module User (
    User, makeUser, totalSize, mostUsedLang, name, followers, hashFollowers, getFollowerNames,
    filterFollowersByHashset
    ) where

import Data.Vector as V (Vector, length, map, null, head, tail, toList, filter)
import GitRepos
import GitFollowers

import Data.HashSet as H


data User =
    User {
        name :: String,
        followers  :: Vector Follower,
        numOfRepos :: Int,
        totalSize :: Int,
        mostUsedLang :: String,
        repos :: Vector Repo
        }

instance Show User where
    show (User name followers numOfRepos totalSize lang repos) = 
                                "name: " ++ name ++ "\n" ++
                                "number of repos: " ++ show numOfRepos ++ "\n" ++
                                "total size: " ++ show totalSize ++ "kb\n" ++
                                "most used language: " ++ lang ++ "\n" ++
                                "repos:\n" ++ concat (V.map show repos) ++
                                "followers:\n" ++ concat (V.map show followers)

makeUser :: String -> Vector Repo -> Vector Follower -> User
makeUser name repos followers = User name
                                     followers
                                     (V.length repos)
                                     (calcSize repos)
                                     (getMostFreqLangFromRepos repos) --TODO write function in repos that takes an array of repos and returns most common Lang
                                     repos

hashFollowers :: [String] -> H.HashSet String
hashFollowers [] = H.empty
hashFollowers (x:xs) = H.insert x (hashFollowers xs)

getFollowerNames :: User -> [String]
getFollowerNames (User _ followers _ _ _ _) = V.toList (V.map getFollowerName followers)

filterFollowersByHashset :: Vector Follower -> Maybe (H.HashSet String) -> Vector Follower
filterFollowersByHashset followers Nothing = followers
filterFollowersByHashset followers (Just hashset) = V.filter (\f -> H.member (getFollowerName f) hashset) followers