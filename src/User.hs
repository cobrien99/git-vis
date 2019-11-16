module User (
    User
    ) where

import Data.Vector as V (Vector, length)
import qualified GitHub.Endpoints.Users.Followers as G
import qualified GitHub.Data.Name as Name
import GitRepos
import GitFollowers


data User =
    User {
        name :: String,
        followers  :: Vector G.SimpleUser,
        numOfRepos :: Int,
        totalSize :: Int,
        mostUsedLang :: String,
        repos :: Vector Repo
        }

{-instance Show User where
    show (User name followers numOfRepos totalSize lang repos) = name ++ " " ++
                                            show isFork ++ " " ++ 
                                            language ++ "\n"-}

makeUser :: String -> Vector Repo -> Vector G.SimpleUser -> User
makeUser name repos followers = User name
                                     followers
                                     (V.length repos)
                                     0 --TODO write function in repos that takes an array of repos and returns most common size
                                     "Haskell" --TODO write function in repos that takes an array of repos and returns most common Lang
                                     repos
