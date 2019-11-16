module GitRepos (
    getUsersRepos, Repo, calcSize)
where

import qualified GitHub.Endpoints.Repos as G
import qualified GitHub.Data.Name as Name
import Data.Text         (pack, unpack)
import Data.Char (toLower)
import Data.Vector as V ( map, Vector, head, tail)
import Data.Foldable (maximumBy)

getUsersRepos :: String -> String -> IO (Maybe (Vector Repo))
getUsersRepos userName publicity = do
    possibleRepos <- G.userRepos (Name.N $ pack userName) (getPublicity publicity)

    case possibleRepos of
       (Left _)  -> return Nothing
       (Right repos) -> return $ Just (V.map makeRepo repos)

getPublicity :: String -> G.RepoPublicity
getPublicity x
            | toLower (Prelude.head x) == 'o' = G.RepoPublicityOwner
            | otherwise = G.RepoPublicityAll

formatLanguage :: Maybe G.Language -> String
formatLanguage (Just language) = unpack $ G.getLanguage language
formatLanguage Nothing = ""

getSize :: Maybe Int -> Int
getSize (Just size) = size
getSize Nothing = 0

getFork :: Maybe Bool -> Bool
getFork (Just x) = x
getFork Nothing = True

data Repo = 
    Repo {
        name :: String,
        size :: Int,
        isAFork :: Bool,
        language :: String
    }

makeRepo :: G.Repo -> Repo
makeRepo x = Repo      (unpack $ G.untagName $ G.repoName x)
                       (getSize $ G.repoSize x)
                       (getFork $ G.repoFork x)
                       (formatLanguage $ G.repoLanguage x)
                        
instance Show Repo where
    show (Repo name size isFork language) = name ++ " " ++
                                            show size ++ "kb " ++ 
                                            show isFork ++ " " ++ 
                                            language ++ "\n"



type LangFreq = (String, Int) 


calcSize :: Vector Repo -> Int
calcSize v
    | null v    = 0
    | otherwise = size (V.head v) + calcSize (V.tail v)


getMostFreqLang :: [LangFreq] -> LangFreq
getMostFreqLang x = maximumBy (\(_, f) (_, f') -> compare f f') x

addLang :: LangFreq -> [LangFreq] -> [LangFreq]
addLang (l,f) [] = [(l,f)]
addLang (l,f) [(a,b)]
    | a == l = [(a, b')]
    | otherwise = (l, f) : [(a, b)] 
    where b' = b + f

addLang (l,f) ((a,b):xs) 
    | a == l = (a, b') : xs
    | otherwise = addLang (l,f) xs
    where b' = b + f
