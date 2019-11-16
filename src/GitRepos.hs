module GitRepos where

import qualified GitHub.Endpoints.Repos as G
import qualified GitHub.Data.Name as Name
import Data.Text         (pack)
import Data.Vector as V

--me = G.userRepos (Name.N $ pack "cobrien99") G.RepoPublicityAll

{-main = do
  possibleRepos <- G.userRepos (Name.N $ pack "cobrien99") G.RepoPublicityAll
  case possibleRepos of
       (Left error)  -> putStrLn $ "Error: " ++ (show error)
       (Right repos) -> putStrLn $ intercalate "\n\n" $ map formatRepo repos

formatRepo repo =
  (Github.repoName repo) ++ "\t" ++
    (fromMaybe "" $ Github.repoDescription repo) ++ "\n" ++
    (Github.repoHtmlUrl repo) ++ "\n" ++
    (fromMaybe "" $ Github.repoCloneUrl repo) ++ "\t" ++
    (formatDate $ Github.repoUpdatedAt repo) ++ "\n" ++
    formatLanguage (Github.repoLanguage repo) ++
    "watchers: " ++ (show $ Github.repoWatchers repo) ++ "\t" ++
    "forks: " ++ (show $ Github.repoForks repo)

formatDate (Just date) = show . Github.fromDate $ date
formatDate Nothing = ""

formatLanguage (Just language) = "language: " ++ language ++ "\t"
formatLanguage Nothing = ""-}

getUsersRepos :: String -> String -> IO String
getUsersRepos userName publicity = do
    possibleRepos <- G.userRepos (Name.N $ pack userName) (getPublicity publicity)

    case possibleRepos of
       (Left error)  -> return $ show error
       (Right repos) -> return $ show $ V.length repos

getPublicity :: String -> G.RepoPublicity
getPublicity x
            | x == "Owner" = G.RepoPublicityOwner
            | otherwise = G.RepoPublicityAll