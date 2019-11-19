module Graphics
    (  
        draw
        ) where

import User (User, totalSize, mostUsedLang, name)
    --TODO
    --convert languages to colours
    --put all the main users followers in a hash set

import Graphics.Gloss as G

draw :: User -> IO ()
draw user = display displaySettings white (makeProfileCircle user)


width, height, offset :: Int
width = 300
height = 300
offset = 100


displaySettings :: G.Display
displaySettings = InWindow "Git-Vis" (width, height) (offset, offset)

makeProfileCircle :: User -> Picture
makeProfileCircle user = Pictures 
                                [translate (0) (0) $ (Color col circle),
                                translate (0) (0) $ name]
    where circle = ThickCircle 0 $ getSizeCircle (User.totalSize user)
          col = getLangColour (User.mostUsedLang user)
          name = (Text $ User.name user)


getSizeCircle :: Int -> Float
getSizeCircle x = (fromIntegral x) / 10

getLangColour :: String -> Color
getLangColour _ = red

