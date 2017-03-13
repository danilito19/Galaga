{- 
   File      :  Sprite.hs 
   Represents the Sprite typeclass, which includes definitions for rendering 
   and updating the game 
-}
module Sprite 
(
    SpriteClass,
    Sprite(..),
    spaceCoords,
    updateState
) where  

import Graphics.Gloss
import GameConstants


class SpriteClass a where
    spaceCoords :: a -> (Size, Point)
    updateState :: a -> State -> a

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
    state :: State
} | Enemy {
    loc :: Point,
    size :: Size,
    elapsedTime :: Float,
    state :: State,
    frame :: Frame,
    enemyColor :: EnemyColor
}

instance Eq Sprite where
    a == b = (loc a) == (loc b)

instance SpriteClass Sprite where
    spaceCoords s = (size s, loc s)
    
    updateState ship@( Ship {state=oldstate}) newState = ship { state = newState }    
    updateState bullet@( Ship {state=oldstate}) newState = bullet { state = newState }    
    updateState enemy@( Ship {state=oldstate}) newState = enemy { state = newState }    
