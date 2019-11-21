module Graphics
    (  
        draw, makeProfileCircle, Graphics.show
        ) where

import User (User, totalSize, mostUsedLang, name, followers, hashFollowers, getFollowerNames)
    --TODO
    --convert languages to colours
    --put all the main users followers in a hash set

import Graphics.Gloss as G

show :: Picture -> IO ()
show picture = display displaySettings white picture



width, height, offset :: Int
width = 1000
height = 1000
offset = 0


displaySettings :: G.Display
displaySettings = InWindow "Git-Vis" (width, height) (offset, offset)

makeProfileCircle :: User -> [UserCoOrd] -> Picture
makeProfileCircle user coOrds = drawPicAt (Pictures ([(Color colour circle),
                                 drawPicAt name (-50,0)] ++ lines)) (x, y)
                        where
                        circle = circleSolid (getSizeCircle (User.totalSize user))
                        colour = getLangColour (User.mostUsedLang user)
                        name = (Text $ User.name user)
                        (x, y) = getUserCoOrds (User.name user) coOrds
                        lines = drawLinesToFollowers (x, y) (getFollowerNames user) coOrds


getSizeCircle :: Int -> Float
getSizeCircle x = log (fromIntegral x) * 100

getLangColour :: String -> Color
getLangColour _ = red

drawPicAt :: Picture -> (Float, Float) -> Picture
drawPicAt pic (x, y) = translate x y pic


type UserCoOrd = (String, (Float, Float))

getOffset :: Int -> Float
getOffset numFollowers = (2 * pi) / fromIntegral numFollowers

calcCoOrds :: [User] -> Float -> (Float, Float) -> [UserCoOrd]
calcCoOrds [] _ _ = []
calcCoOrds (user:users) offset (prevX, prevY) =
                (name, (x, y)) : calcCoOrds users offset (x, y)
                where   x = prevX + 1000 * cos offset 
                        y = prevY + 1000 * sin offset
                        name = User.name user

getUserCoOrds :: String -> [UserCoOrd] -> (Float, Float)
getUserCoOrds _ [] = (0,0)
getUserCoOrds name (u:us)
            | fst u == name = snd u
            | otherwise = getUserCoOrds name us



draw :: [User] -> Picture
draw [] = Blank
draw users = Pictures (map (`makeProfileCircle` coOrds) users)
    where   offset = getOffset (length users)
            coOrds = calcCoOrds users offset (0, 100)


drawLinesToFollowers :: (Float, Float) -> [String] -> [UserCoOrd] -> [Picture]
drawLinesToFollowers _ [] _ = []
drawLinesToFollowers (x, y) (name:names) coOrds = Line [(x,y), (getUserCoOrds name coOrds)]
                                                    : drawLinesToFollowers (x, y) names coOrds
