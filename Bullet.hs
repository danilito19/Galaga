module Bullet 
(
   render,
   update,
   addBullet
) where 

import Graphics.Gloss
import GameConstants
import Collision
import Sprite

bulletBoxBound :: Float
bulletBoxBound = 5

shipBulletPic :: IO Picture  
shipBulletPic = loadBMP "images/models/ship_bullet.bmp"

mkBullet :: Sprite 
mkBullet = Bullet (0, 0) (200,200) 0 Alive

addBullet :: [Sprite] -> [Sprite]
addBullet bullets = mkBullet:bullets

render :: [Sprite] -> IO Picture 
render [] = return Blank
render bullets = do
    pic <- shipBulletPic
    let bullet = bullets !! 0
    return $ scale 2 2 $ translate (fst (loc bullet)) (snd (loc bullet)) pic

update :: Float -> Point -> [Sprite] -> [Sprite]
update _ _ [] = []
update deltaTime shipLoc bullets =

    let bullet = bullets !! 0
        elTime = (elapsedTime bullet) + deltaTime  
        distance = calcDistance deltaTime
    in 
    if elTime >= 0.5
      then
        [Bullet ((fst shipLoc), (snd shipLoc) + distance) (size bullet) 0 Alive]
      else
        [Bullet ((fst shipLoc), (snd shipLoc)) (size bullet) elTime Alive]

      -- return [] if a while passed or if loc > screen
    -- where
    --     distance = calcDistance deltaTime

