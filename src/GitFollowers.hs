{-# LANGUAGE NoImplicitPrelude #-}

module GitFollowers
    ( githubFollowers
    ) where

import Prelude.Compat 

import Data.Text         (pack)

import qualified GitHub.Endpoints.Users.Followers as G
import qualified GitHub.Data.Name as Name

import Data.Vector as V (Vector)

--takes a username and returns an array of the users following that username
githubFollowers :: String -> IO (Maybe (Vector G.SimpleUser))
githubFollowers userName = do
    possibleUsers <- G.usersFollowing (Name.N $ pack userName)

    case possibleUsers of
       (Left error)  -> return Nothing
       (Right repos) -> return $ Just repos