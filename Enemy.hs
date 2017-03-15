module Enemy
(
   render,
   mkInitEnemies,
   initEnemyLocations,
   update,
   filterDeadEnemies
) where

import Graphics.Gloss
import GameConstants
import Collision
import Sprite

enemyDefaultSize :: Size
enemyDefaultSize = (30,30)

-- enemy pics for different states
getPic :: Sprite -> IO Picture
getPic enemy@( Enemy {frame=F1}) = loadBMP "images/explosion/enemy/frame1.bmp"
getPic enemy@( Enemy {frame=F2}) = loadBMP "images/explosion/enemy/frame2.bmp"
getPic enemy@( Enemy {frame=F3}) = loadBMP "images/explosion/enemy/frame3.bmp"
getPic enemy@( Enemy {enemyColor=Red}) = loadBMP "images/models/red_fighter.bmp"
getPic enemy@( Enemy {enemyColor=Blue}) = loadBMP "images/models/blue_fighter.bmp"

-- initial enemy locations in the game
initEnemyLocations :: [Point]
initEnemyLocations = [(-200, 300),(-100, 300), (0,300),  (100, 300), (200, 300), -- red fighters
                      (-200, 200),(-100, 200), (0,200),  (100, 200), (200, 200)] -- blue fighters]

-- make a red or blue enemy based on location
mkInitEnemy :: Float -> Float -> Sprite
mkInitEnemy x y = if y == 300
    then
      Enemy (x, y) enemyDefaultSize 0 Alive Normal Red
    else
      Enemy (x, y) enemyDefaultSize 0 Alive Normal Blue

--- make enemies given a list of locations
mkInitEnemies :: [Sprite]
mkInitEnemies = map (\loc -> mkInitEnemy (fst loc) (snd loc) ) initEnemyLocations

-- render one enemy
getEnemy :: Sprite -> IO Picture
getEnemy enemy = do
    pic <- getPic enemy
    return $ translate (fst (loc enemy)) (snd (loc enemy)) pic

-- render a list of enemies
render :: [Sprite] -> IO [IO Picture]
render enemies = do
    return $ map (\e -> getEnemy e) enemies

-- change the y coordinate location of an enemy
moveDown :: Float -> Point -> Point
moveDown deltaTime (x, y) = (x, y-distance)
    where
      distance = calcDistance deltaTime

-- updates the Enemies' location
update :: Float -> [Sprite] -> [Sprite]
update deltaTime enemies  =
    let elTime = (elapsedTime (enemies !! 0)) + deltaTime
    in

    if elTime >= 1
    then
        updateEnemies 0 enemies $ map (\e -> moveDown deltaTime $ loc e) enemies
    else
        updateEnemies elTime enemies $ map (\e -> loc e) enemies

-- update all enemies
updateEnemies :: Float -> [Sprite] -> [Point] -> [Sprite]
updateEnemies elapsedTime enemies locations =
  map (\(e, loc) -> updateEnemy elapsedTime (fst loc, snd loc) e ) $ zip enemies locations

-- update a single enemy depending on state or location
updateEnemy :: Float -> Point -> Sprite -> Sprite
updateEnemy elapsedTime loc ( Enemy _ size _ state frame enemyColor) =
  if snd loc < -200 
    then
       Enemy loc size elapsedTime Dead frame enemyColor
    else
      case state of 
        Exploding -> updateFrame $ Enemy loc size elapsedTime state frame enemyColor
        otherwise -> Enemy loc size elapsedTime state frame enemyColor

-- remove dead enemies from enemies list
filterDeadEnemies :: [Sprite] -> [Sprite]
filterDeadEnemies enemies =
  filter (\e -> state e /= Dead) enemies
