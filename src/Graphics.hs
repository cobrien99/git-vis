module Graphics
    (  
        draw, makeProfileCircle, Graphics.show
        ) where

import User (User, totalSize, mostUsedLang, name, followers, hashFollowers, getFollowerNames)

import Graphics.Gloss as G
import LangColours (langToHexColour, Object)

show :: Picture -> IO ()
show picture = display displaySettings white picture



width, height, offset :: Int
width = 1000
height = 1000
offset = 0


displaySettings :: G.Display
displaySettings = InWindow "Git-Vis" (width, height) (offset, offset)

makeProfileCircle :: User -> [UserCoOrd] -> Object -> Picture
makeProfileCircle user coOrds colourJson =  Pictures (lines ++ [drawPicAt (Color colour circle) (x, y),
                                 drawPicAt name (x, y+500)])
                        where
                        circle = circleSolid (getSizeCircle (User.totalSize user))
                        colour = getLangColour (User.mostUsedLang user) colourJson
                        name = Scale 5 5 (Text $ User.name user)
                        (x, y) = getUserCoOrds (User.name user) coOrds
                        lines = drawLinesToFollowers (x, y) (getFollowerNames user) coOrds


getSizeCircle :: Int -> Float
getSizeCircle x = log (fromIntegral x) * 100

getLangColour :: String -> Object -> Color
getLangColour langName json = makeColorI r g b a
    where   colour = langToHexColour langName json
            r = colour !! 0
            g = colour !! 1
            b = colour !! 2
            a = colour !! 3

drawPicAt :: Picture -> (Float, Float) -> Picture
drawPicAt pic (x, y) = translate x y pic


type UserCoOrd = (String, (Float, Float))

getOffset :: Int -> Float
getOffset numFollowers = (2 * pi) / fromIntegral numFollowers

calcCoOrds :: [User] -> Float -> (Float, Float) -> [UserCoOrd]
calcCoOrds [user] _ _ = [(User.name user, (0,0))]
calcCoOrds (user:users) offset (prevX, prevY) =
                (name, (x, y)) : calcCoOrds users offset (prevX + offset, prevY + offset)
                where   x = 20000 * cos ( prevX + offset )
                        y = 20000 * sin ( prevY + offset )
                        name = User.name user

getUserCoOrds :: String -> [UserCoOrd] -> (Float, Float)
getUserCoOrds _ [] = (0,0)
getUserCoOrds name (u:us)
            | fst u == name = snd u
            | otherwise = getUserCoOrds name us






draw :: [User] -> Object -> Picture
draw [] _ = Blank
draw users colourJson= Pictures (map (\x -> makeProfileCircle x coOrds colourJson) users)
    where   offset = getOffset (length users)
            coOrds = calcCoOrds users offset (100, 0)


drawLinesToFollowers :: (Float, Float) -> [String] -> [UserCoOrd] -> [Picture]
drawLinesToFollowers _ [] _ = []
drawLinesToFollowers (x, y) (name:names) coOrds = Line [(x,y), (getUserCoOrds name coOrds)]
                                                    : drawLinesToFollowers (x, y) names coOrds
  