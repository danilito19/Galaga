module Ship
(
   mkShip,
   render,
   update,
   mkLives,
   ShipLives,
   renderLives
) where

import Debug.Trace
import Graphics.Gloss
import GameConstants
import Collision
import Sprite

type ShipLives = [Sprite]

shipDefaultSize :: Size
shipDefaultSize = (30,30)


--ship pictures for different states
getPic :: Frame -> IO Picture
getPic Normal = loadBMP "images/models/ship.bmp"
getPic F1 = loadBMP "images/explosion/ship/frame1.bmp"
getPic F2 = loadBMP "images/explosion/ship/frame2.bmp"
getPic F3 = loadBMP "images/explosion/ship/frame3.bmp"

-- render a ship to the screen
render :: Sprite -> IO Picture
render ship@(Ship {frame = fr}) = do
    pic <- getPic fr
    return $ translate (fst (loc ship)) (snd (loc ship)) pic

-- render the player's lives as ships
renderLives :: ShipLives ->  IO [IO Picture]
renderLives lives = do
    return $ map (\l -> render l) lives

-- make one ship that's Alive
mkShip :: Sprite
mkShip = Ship (0, -150) shipDefaultSize NotMoving 0.0 Alive Normal

-- make three ship lives 
mkLives :: ShipLives
mkLives = [
          Ship (-320, -300) (0,0) NotMoving 0.0 Alive Normal,
          Ship (-280, -300) (0,0) NotMoving 0.0 Alive Normal,
          Ship (-240, -300) (0,0) NotMoving 0.0 Alive Normal
         ]

-- update a ship's properties
update :: Float -> Sprite -> Sprite
update deltaTime ship@( Ship loc size direction elapsedTime Exploding frame ) =
    let elTime = elapsedTime + deltaTime
    in
      if elTime > 0.2 then
        updateFrame ship
      else
        Ship loc size direction elTime Exploding frame 

update deltaTime ship@( Ship (x,y) size direction elapsedTime state frame) =
    let distance = calcDistance deltaTime
    in
        case direction of
            MoveLeft -> (Ship ( x-distance, y) (size) NotMoving elapsedTime state frame)
            MoveRight -> (Ship ( x+distance, y) (size) NotMoving elapsedTime state frame)
            _ -> (Ship (x,y) (size) (direction) 0.0 state frame)

