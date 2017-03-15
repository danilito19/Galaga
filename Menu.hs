module Menu 
(
   render
) where 

import Graphics.Gloss
import GameConstants 


galagaPic :: IO Picture  
galagaPic = loadBMP "images/models/galaga.bmp"

-- render the main menu
render :: IO Picture 
render = do 
    pic <- galagaPic

    return( pictures 
                [scale 0.5 0.5 $ pic,
                 scale 0.25 0.25 $ translate (-500) (-800) $ color white (text $ "Press p to play"),
                 scale 0.25 0.25 $ translate (-500) (-1000) $ color white (text $ "Press h see high scores")
                 ] 
            )