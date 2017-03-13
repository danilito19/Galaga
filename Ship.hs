module Ship 
(
   mkShip, 
   render,
   update,
   mkLives,
   ShipLives,
   renderLives,
   renderExplode
) where 

import Graphics.Gloss
import GameConstants
import Collision
import Sprite

type ShipLives = [Sprite]

shipBoundBox :: Float
shipBoundBox = 5

shipPic :: IO Picture
shipPic = loadBMP "images/models/ship.bmp"

getPic :: Frame -> IO Picture
getPic Normal = return Blank
getPic F1 = loadBMP "images/explosion/ship/frame1.bmp"
getPic F2 = loadBMP "images/explosion/ship/frame2.bmp"
getPic F3 = loadBMP "images/explosion/ship/frame3.bmp"

renderExplode :: Point -> Frame -> IO Picture
renderExplode shipLoc frame = do
  pic <- getPic frame
  return $ scale 2 2 $ translate (fst shipLoc) (snd shipLoc) pic

render :: Sprite -> IO Picture 
render ship = do 
    pic <- shipPic
    return $ scale 2 2 $ translate (fst (loc ship)) (snd (loc ship)) pic

renderLives :: ShipLives ->  IO [IO Picture]
-- renderLives [] = return [Blank, Blank, Blank]
renderLives lives = do
    return $ map (\l -> render l) lives

mkShip :: Sprite 
mkShip = Ship (0, -150) (0,0) NotMoving 0.0 Alive Normal

mkLives :: ShipLives
mkLives = [
          Ship (-180, -150) (0,0) NotMoving 0.0 Alive Normal,
          Ship (-140, -150) (0,0) NotMoving 0.0 Alive Normal,
          Ship (-100, -150) (0,0) NotMoving 0.0 Alive Normal
         ]
  
update :: Float -> Sprite -> Sprite
update deltaTime ship@( Ship loc size direction elapsedTime Exploding frame ) = 
    let elTime = elapsedTime + deltaTime  
    in
      if elTime > 0.3 then
        updateFrame ship
      else 
        ship


update deltaTime ship@( Ship (x,y) size direction elapsedTime state frame) =
    let distance = calcDistance deltaTime
    in
        case direction of
            MoveLeft -> (Ship ( x-distance, y) (size) NotMoving elapsedTime state frame)
            MoveRight -> (Ship ( x+distance, y) (size) NotMoving elapsedTime state frame)
            _ -> (Ship (x,y) (size) (direction) 0.0 state frame)
 

updateFrame :: Sprite -> Sprite
updateFrame ship@( Ship {state = oldState, frame = oldFrame}) 
    | oldFrame == Normal = ship { state = Exploding, frame = F1 }
    | oldFrame == F1 = ship { state = Exploding, frame = F2 }
    | oldFrame == F2 = ship { state = Exploding, frame = F3 }
    | otherwise = ship { state = Dead, frame = Normal }

