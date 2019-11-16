{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Git
    ( githubFollowers
    ) where

import Prelude.Compat 

import Data.Text         (pack, unpack)
import Data.Monoid       ((<>))

import qualified GitHub.Endpoints.Users.Followers as G
import qualified GitHub.Data.Name as Name

githubFollowers :: String -> IO String
githubFollowers userName = do
    possibleUsers <- G.usersFollowing (Name.N $ pack userName)
    return $ either show
                        (foldMap ((<> "\n") . formatUser))
                        possibleUsers

formatUser :: G.SimpleUser -> String
formatUser = unpack . G.untagName . G.simpleUserLogin