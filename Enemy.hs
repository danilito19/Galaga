module Enemy 
(
   render,
   mkInitEnemies,
   initEnemyLocations,
   update
) where

import Graphics.Gloss
import GameConstants
import Collision
import Sprite


redOrBlue :: Sprite -> IO Picture
redOrBlue enemy@( Enemy {enemyColor=Red}) = loadBMP "images/models/red_fighter.bmp"
redOrBlue enemy@( Enemy {enemyColor=Blue}) = loadBMP "images/models/blue_fighter.bmp"

initEnemyLocations :: [Point]
initEnemyLocations = [(-200, 300),(-100, 300), (0,300),  (100, 300), (200, 300), -- red fighters
                      (-200, 200),(-100, 200), (0,200),  (100, 200), (200, 200) -- blue fighters
                      ] 

mkInitEnemy :: Float -> Float -> Sprite 
mkInitEnemy x y = if x == 300 
    then
      Enemy (x, y) (5,5) 0 Alive Normal Red
    else
      Enemy (x, y) (5,5) 0 Alive Normal Blue

--- make enemies given a list of locations
mkInitEnemies :: [Sprite]
mkInitEnemies = map (\loc -> mkInitEnemy (fst loc) (snd loc) ) initEnemyLocations

getEnemy :: Sprite -> IO Picture 
getEnemy enemy = do 
    pic <- redOrBlue enemy
    return $ scale 0.25 0.25 $ translate (fst (loc enemy)) (snd (loc enemy)) pic

render :: [Sprite] -> IO [IO Picture]
render enemies = do
    return $ map (\e -> getEnemy e) enemies

moveDown :: Float -> Point -> Point
moveDown deltaTime (x, y) = (x, y-distance)
    where
      distance = calcDistance deltaTime

-- updates the Enemies' location
update :: Float -> [Sprite] -> [Sprite]
update deltaTime enemies  =  
    let elTime = (elapsedTime (enemies !! 0)) + deltaTime  
    in 

    if elTime >= 1.5
    then 
        updateEnemies 0 enemies $ map (\e -> moveDown deltaTime $ loc e) enemies
    else  
        updateEnemies elTime enemies $ map (\e -> loc e) enemies


updateEnemies :: Float -> [Sprite] -> [Point] -> [Sprite]
updateEnemies elapsedTime enemies locations = 
  map (\(e, loc) -> updateEnemy elapsedTime (fst loc, snd loc) e ) $ zip enemies locations

updateEnemy :: Float -> Point -> Sprite -> Sprite
updateEnemy elapsedTime loc enemy@( Enemy {elapsedTime=e, loc=l}) =
    enemy { elapsedTime = elapsedTime, loc = loc }


