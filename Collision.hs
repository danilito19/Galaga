module Collision 
(
    SpriteClass(..),
    detectCollision
) where

import Sprite
import Graphics.Gloss
import GameConstants

twoCollided :: (SpriteClass a, Eq a) => a -> a -> Bool
twoCollided a b =
    (abs $ aX - bX) * 2 < (aWidth + bWidth) &&
    (abs $ aY - bY) * 2 < (aHeight + bHeight)
    where
        ((aHeight, aWidth), (aX, aY)) = spaceCoords a
        ((bHeight, bWidth), (bX, bY)) = spaceCoords b

detectCollision :: (SpriteClass a,  Eq a) => [a] -> [a]
detectCollision [] = []
detectCollision sprites = map (\x -> updateOrNot sprites x) sprites

updateOrNot :: (SpriteClass a, Eq a) => [a] -> a -> a
updateOrNot sprites x =  case anyCollided sprites x of 
        True -> updateState x Exploding
        otherwise -> x

anyCollided :: (SpriteClass a, Eq a) => [a] -> a -> Bool
anyCollided [] _ = False
anyCollided (x:xs) el = 
    if x == el then 
        anyCollided xs el 
    else 
        case twoCollided x el of 
            True -> True
            False -> anyCollided xs el

