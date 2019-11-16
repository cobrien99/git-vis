module GitRepos (
    getUsersRepos)
where

import qualified GitHub.Endpoints.Repos as G
import qualified GitHub.Data.Name as Name
import Data.Text         (pack, unpack)
import Data.Char (toLower)
import Data.Vector as V (length, map, Vector)

{-getUsersRepos :: String -> String -> IO String
getUsersRepos userName publicity = do
    possibleRepos <- G.userRepos (Name.N $ pack userName) (getPublicity publicity)

    case possibleRepos of
       (Left error)  -> return $ show error
       (Right repos) -> return $ formatOutput userName (V.length repos) publicity ++
            concat (V.map parseRepo repos)-}

getUsersRepos :: String -> String -> IO (Maybe (Vector Repo))
getUsersRepos userName publicity = do
    possibleRepos <- G.userRepos (Name.N $ pack userName) (getPublicity publicity)

    case possibleRepos of
       (Left error)  -> return $ Nothing
       (Right repos) -> return $ Just (V.map makeRepo repos)

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
parseRepo repo = 
    show (G.repoName repo) ++ " " ++
    formatSize (G.repoSize repo) ++ " " ++
    formatFork (G.repoFork repo) ++ " " ++
    formatLanguage (G.repoLanguage repo) ++ "\n"

formatLanguage :: Maybe G.Language -> String
formatLanguage (Just language) = unpack $ G.getLanguage language
formatLanguage Nothing = ""

formatSize :: Maybe Int -> String
formatSize (Just size) = (++) (show size) "kb"
formatSize Nothing = ""

getSize :: Maybe Int -> Int
getSize (Just size) = size
getSize Nothing = 0

formatFork :: Maybe Bool -> String
formatFork (Just x) =
    if x then
        "Fork" else
        "Not a Fork"
formatFork Nothing = ""

getFork :: Maybe Bool -> Bool
getFork (Just x) = x
getFork Nothing = False

data Repo = 
    Repo {
        name :: String,
        size :: Int,
        isAFork :: Bool,
        language :: String
    } deriving (Show)

makeRepo :: G.Repo -> Repo
makeRepo x = Repo      (unpack $ G.untagName $ G.repoName x)
                       (getSize $ G.repoSize x)
                       (getFork $ G.repoFork x)
                       (formatLanguage $ G.repoLanguage x)
                        