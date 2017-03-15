module Bullet
(
   render,
   update,
   addShipBullet, 
   fireEnemyBullets
) where

import Graphics.Gloss
import GameConstants
import Collision
import Sprite
import System.Random (randomRIO)


bulletDefaultSize :: Size
bulletDefaultSize = (30,30)

-- bullet images depending on owner
shipOrFighterPic :: Sprite -> IO Picture
shipOrFighterPic bullet@( Bullet {owner=Player}) = loadBMP "images/models/ship_bullet.bmp"
shipOrFighterPic bullet@( Bullet {owner=Fighter}) = loadBMP "images/models/enemy_bullet.bmp"

-- make one bullet given size and owner
mkBullet :: Point -> Owner -> Sprite
mkBullet loc owner = Bullet loc bulletDefaultSize 0 Alive owner

-- add a ship bullet to a list of bullets
addShipBullet :: [Sprite] -> Point -> [Sprite]
addShipBullet bullets loc = (mkBullet loc Player):bullets

-- add an enemy bullet to a list of bullets
addFighterBullet :: [Sprite] -> Point -> [Sprite]
addFighterBullet bullets loc = (mkBullet loc Fighter):bullets

--render all bullets
render :: [Sprite] -> IO [IO Picture]
render bullets = do
    return $ map (\b -> getBulletPic b) bullets

-- render one bullet
getBulletPic :: Sprite -> IO Picture
getBulletPic bullet = do
    pic <- shipOrFighterPic bullet
    return $ translate (fst (loc bullet)) (snd (loc bullet)) pic

-- remove any dead or Exploding bullets
filterDeadBullets :: [Sprite] -> [Sprite]
filterDeadBullets bullets =
  filter (\e -> state e /= Dead && state e /= Exploding) bullets

-- update all bullets
update :: Float -> [Sprite] -> [Sprite]
update _ [] = []
update deltaTime bullets = 
  filterDeadBullets $ map (\b -> updateBullet deltaTime b) bullets

-- update a single bullet based on location and owner
updateBullet :: Float -> Sprite -> Sprite
updateBullet deltaTime bullet@(Bullet (x, y) size elapsedTime state owner) =
  let
    elTime = elapsedTime + deltaTime
    distance = calcDistance deltaTime
  in

    if y < - 200
       then Bullet (x, y) size 0 Dead owner
       else
          if elTime >= 0.2
            then
              case owner of 
                Player -> Bullet (x, y + distance) size 0 Alive owner
                Fighter -> Bullet (x, y - distance) size 0 Alive owner
            else
              Bullet (x, y) size elTime Alive owner

-- pick a random enemy's location
randomEnemyLoc :: [Sprite] -> IO Point
randomEnemyLoc enemies = do
  idx <- randomRIO (0, length enemies -1)
  return $ loc (enemies !! idx)

-- create an enemy bullet
fireEnemyBullets :: [Sprite] -> [Sprite] -> IO [Sprite]
fireEnemyBullets enemies bullets = 
    if length enemyBullets < 2 
      then do
        loc <- randomEnemyLoc enemies
        return $ addFighterBullet bullets ((fst loc - 10), (snd loc - 50))
      else do
        return bullets
    where
      enemyBullets = [b | b@Bullet{owner=Fighter} <- bullets]

