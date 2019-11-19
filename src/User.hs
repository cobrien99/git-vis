module User (
    User, makeUser, totalSize, mostUsedLang, name
    ) where

import Data.Vector as V (Vector, length, map, null, head, tail)
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
                                "followers:\n" ++ concat (V.map show followers) ++
                                "hashed: " ++ show (hashFollowers followers)

makeUser :: String -> Vector Repo -> Vector Follower -> User
makeUser name repos followers = User name
                                     followers
                                     (V.length repos)
                                     (calcSize repos)
                                     (getMostFreqLangFromRepos repos) --TODO write function in repos that takes an array of repos and returns most common Lang
                                     repos

hashFollowers :: Vector Follower -> H.HashSet String
hashFollowers v
            | V.null v  = H.empty
            | otherwise = H.insert (getFollowerName follower) (hashFollowers $ V.tail v)
            where follower = V.head v :: Follower