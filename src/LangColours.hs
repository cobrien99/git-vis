module LangColours (
        loadJSON, langToHexColour, Object
    )
where

import Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import Data.Text as T
import Data.HashMap.Strict as H
import Data.Char as C


jsonFile :: FilePath
jsonFile = "/home/cathal/college/sweng/git-vis/langColors.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

handleMaybe :: Maybe a -> a
handleMaybe (Just a) = a

handleMaybe2 :: Maybe Value -> Value
handleMaybe2 (Just a) = a
handleMaybe2 Nothing = Null

loadJSON :: IO Object
loadJSON = do 
            json <- getJSON
            let result = A.decode json :: Maybe Object
            return (handleMaybe result)

langToHexColour :: String -> Object -> [Int]
langToHexColour langName = hexColourToRGB .stringifyVal . handleMaybe2 . H.lookup (T.pack langName)

stringifyVal :: Value -> String
stringifyVal (String x) = unpack x
stringifyVal Null = "#000000" --base case error handling 

hexColourToRGB :: String -> [Int] --string in form #RRGGBB
hexColourToRGB colour = [r,g,b, 255] --255 represents alpha which will always be max
    where   r = hexToInt [colour !! 1, colour !! 2]
            g = hexToInt [colour !! 3, colour !! 4]
            b = hexToInt [colour !! 5, colour !! 6]


hexToInt :: String -> Int
hexToInt [a,b] = (C.digitToInt a)*16 + (C.digitToInt b)