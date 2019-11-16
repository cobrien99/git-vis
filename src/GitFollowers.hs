{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module GitFollowers
    ( githubFollowers
    ) where

import Prelude.Compat 

import Data.Text         (pack, unpack)
import Data.Monoid       ((<>))

import qualified GitHub.Endpoints.Users.Followers as G
import qualified GitHub.Data.Name as Name

--takes a username and returns an array of the users following that username
githubFollowers :: String -> IO [String]
githubFollowers userName = do
    possibleUsers <- G.usersFollowing (Name.N $ pack userName)
    return $ words $ either show
                        (foldMap ((<> " ") . formatUser))
                        possibleUsers

formatUser :: G.SimpleUser -> String
formatUser = unpack . G.untagName . G.simpleUserLogin