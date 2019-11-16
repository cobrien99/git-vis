module GitRepos (
    getUsersRepos)
where

import qualified GitHub.Endpoints.Repos as G
import qualified GitHub.Data.Name as Name
import Data.Text         (pack)
import Data.Char (toLower)
import Data.Vector as V (length, map)

getUsersRepos :: String -> String -> IO String
getUsersRepos userName publicity = do
    possibleRepos <- G.userRepos (Name.N $ pack userName) (getPublicity publicity)

    case possibleRepos of
       (Left error)  -> return $ show error
       (Right repos) -> return $ formatOutput userName (V.length repos) publicity ++
          concat (V.map parseRepo repos)

getPublicity :: String -> G.RepoPublicity
getPublicity x
            | toLower (head x) == 'o' = G.RepoPublicityOwner
            | otherwise = G.RepoPublicityAll

--formatOutput :: String -> Int -> String -> String
formatOutput :: Show a => String -> a -> String -> String
formatOutput name numOfRepos pub = 
    if toLower (head pub) == 'o' then
        name ++ " owns " ++ show numOfRepos ++ " Repos\n" else
        name ++ " has " ++ show numOfRepos ++ " Repos\n"

parseRepo :: G.Repo -> String
parseRepo repo = show (G.repoName repo) ++ "\n"