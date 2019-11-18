{-# LANGUAGE NoImplicitPrelude #-}

module GitFollowers
    ( githubFollowers, Follower
    ) where

import Prelude.Compat 

import Data.Text         (pack, unpack)

import qualified GitHub.Endpoints.Users.Followers as G
import qualified GitHub.Data.Name as Name
import qualified GitHub.Request as R (executeRequestMaybe)

import Data.Vector as V (Vector, map)

--takes a username and returns an array of the users following that username
githubFollowers :: String -> Maybe G.Auth-> IO (Maybe (Vector Follower))
githubFollowers userName auth = do
    possibleUsers <- usersFollowing' (auth) (Name.N $ pack userName)

    case possibleUsers of
       (Left error)  -> return Nothing
       (Right repos) -> return $ Just (V.map makeFollower repos)

data Follower = 
    Follower {
        name :: String,
        id :: Int,
        avatarUrl :: String,
        profileUrl :: String
    }

makeFollower :: G.SimpleUser -> Follower
makeFollower x = Follower      (unpack $ G.untagName $ G.simpleUserLogin x)
                       (G.untagId $ G.simpleUserId x)
                       (unpack    $ G.getUrl $ G.simpleUserAvatarUrl x)
                       (unpack    $ G.getUrl $ G.simpleUserUrl x)

instance Show Follower where
    show (Follower name id _ _) = "name: " ++ name ++ " " ++
                                    "id: " ++ show id ++ "\n"

usersFollowing' :: Maybe G.Auth -> Name.Name G.User -> IO (Either G.Error (Vector G.SimpleUser))
usersFollowing' auth user = R.executeRequestMaybe auth $ G.usersFollowingR user G.FetchAll