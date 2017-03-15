module Collision
(
    SpriteClass(..),
    detectCollision
) where

import Sprite
import Graphics.Gloss
import GameConstants

-- determine if two objects have collided based on their coordinates
twoCollided :: (SpriteClass a) => a -> a -> Bool
twoCollided a b =
    (abs $ aX - bX) * 2 < (aWidth + bWidth) &&
    (abs $ aY - bY) * 2 < (aHeight + bHeight)
    where
        ((aHeight, aWidth), (aX, aY)) = spaceCoords a
        ((bHeight, bWidth), (bX, bY)) = spaceCoords b

-- map over sprites to see if any have collided
detectCollision :: (SpriteClass a) => [a] -> [a]
detectCollision [] = []
detectCollision sprites = map (\x -> updateOrNot sprites x) sprites

-- determine if a sprite has collided with any other and update its state
updateOrNot :: (SpriteClass a) => [a] -> a -> a
updateOrNot sprites x =  case anyCollided sprites x of
        True -> updateState x Exploding
        otherwise -> x

-- recursively check if objects have collided if they are not the same
anyCollided :: (SpriteClass a) => [a] -> a -> Bool
anyCollided [] _ = False
anyCollided (x:xs) el = do
    if ignoreCollide x el then 
        anyCollided xs el
    else
        case twoCollided x el of
            True -> True
            False -> anyCollided xs el

