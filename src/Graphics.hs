module Graphics
    (  
        draw, makeProfileCircle
        ) where

import User (User, totalSize, mostUsedLang, name, followers, hashFollowers)
    --TODO
    --convert languages to colours
    --put all the main users followers in a hash set

import Graphics.Gloss as G

draw :: Picture -> IO ()
draw picture = display displaySettings white picture



width, height, offset :: Int
width = 1000
height = 1000
offset = 0


displaySettings :: G.Display
displaySettings = InWindow "Git-Vis" (width, height) (offset, offset)

makeProfileCircle :: IO (User) -> IO (Picture)
makeProfileCircle u = do
                    user <- u
                    let circle = circleSolid (getSizeCircle (User.totalSize user))
                    let colour = getLangColour (User.mostUsedLang user)
                    let name = (Text $ User.name user)
                    return (Pictures [translate (0) (0) $ (Color colour circle),
                                 Scale 0.5 0.5 $ translate (-50) 0 name])
          
          


getSizeCircle :: Int -> Float
getSizeCircle x = log (fromIntegral x) * 50

getLangColour :: String -> Color
getLangColour _ = red

