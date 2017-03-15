{-
   File      :  Sprite.hs
   Represents the Sprite typeclass, which includes definitions for rendering
   and updating the game
-}
module Sprite
(
    SpriteClass,
    Sprite(..),
    updateFrame,
    spaceCoords,
    updateState,
    ignoreCollide,
    filterSprites
) where

import Graphics.Gloss
import GameConstants

-- a class to represent game sprites
class SpriteClass a where
    spaceCoords :: a -> (Size, Point)
    updateState :: a -> State -> a
    updateFrame :: a -> a
    ignoreCollide :: a-> a -> Bool

-- a data type to represent ships, bullets, enemies
data Sprite = Ship {
    loc :: Point,
    size :: Size,
    direction :: Direction,
    elapsedTime :: Float,
    state :: State,
    frame :: Frame
} | Bullet {
    loc :: Point,
    size :: Size,
    elapsedTime :: Float,
    state :: State,
    owner :: Owner
} | Enemy {
    loc :: Point,
    size :: Size,
    elapsedTime :: Float,
    state :: State,
    frame :: Frame,
    enemyColor :: EnemyColor
} deriving Show


instance SpriteClass Sprite where
    -- object size and coordinates
    spaceCoords s = (size s, loc s)

    -- update a Sprite's state
    updateState ship@( Ship {state=oldstate}) newState = case oldstate of
        Alive -> ship { state = newState }
        otherwise -> ship { state = oldstate }
    updateState bullet@( Bullet {state=oldstate}) newState = case oldstate of
        Alive -> bullet { state = newState }
        otherwise -> bullet { state = oldstate }
    updateState enemy@( Enemy {state=oldstate}) newState = case oldstate of
        Alive -> enemy { state = newState }
        otherwise -> enemy { state = oldstate }

    -- update a Sprite's frame
    updateFrame ship@( Ship {state = oldState, frame = oldFrame, elapsedTime = oldTime})
        | oldFrame == Normal = ship { state = Exploding, frame = F1, elapsedTime = 0}
        | oldFrame == F1 = ship { state = Exploding, frame = F2, elapsedTime = 0 }
        | oldFrame == F2 = ship { state = Exploding, frame = F3, elapsedTime = 0 }
        | otherwise = ship { state = Dead, frame = Normal, elapsedTime = 0 }
    updateFrame enemy@( Enemy {state = oldState, frame = oldFrame, elapsedTime = oldTime})
        | oldFrame == Normal = enemy { state = Exploding, frame = F1, elapsedTime = 0}
        | oldFrame == F1 = enemy { state = Exploding, frame = F2, elapsedTime = 0 }
        | oldFrame == F2 = enemy { state = Exploding, frame = F3, elapsedTime = 0 }
        | otherwise = enemy { state = Dead, frame = Normal, elapsedTime = 0 }

    -- don't check collision of same Sprite or if ship/enemy and its bullet
    ignoreCollide (Enemy _ _ _ _ _ _) (Enemy _ _ _ _ _ _) = True
    ignoreCollide (Ship _ _ _ _ _ _) (Ship _ _ _ _ _ _) = True
    ignoreCollide (Bullet _ _ _ _ _) (Bullet _ _ _ _ _) = True
    ignoreCollide (Enemy _ _ _ _ _ _) bullet@(Bullet {owner=Fighter}) = True
    ignoreCollide bullet@(Bullet {owner=Fighter}) (Enemy _ _ _ _ _ _) = True
    ignoreCollide (Ship _ _ _ _ _ _) bullet@(Bullet {owner=Player}) = True
    ignoreCollide bullet@(Bullet {owner=Player}) (Ship _ _ _ _ _ _) = True
    ignoreCollide _ _ = False

-- given a list of mixed sprites, return ship, bullets, and enemies in that order
filterSprites :: [Sprite] -> (Sprite, [Sprite], [Sprite])
filterSprites sprites = (newShip, newBullets, newEnemies)
    where
      (newShip:_) = [s | s@Ship{} <- sprites]
      newBullets = [b | b@Bullet{} <- sprites]
      newEnemies = [e | e@Enemy{} <- sprites]
