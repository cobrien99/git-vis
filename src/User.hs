module User (
    User
    ) where

import Data.Vector
import qualified GitHub.Endpoints.Users.Followers as G
import qualified GitHub.Data.Name as Name


data User =
    User {
        name :: String,
        followers  :: Vector G.SimpleUser,
        numOfRepos :: Int,
        totalSize :: Int,
        mostUsedLang :: G.Language
        }