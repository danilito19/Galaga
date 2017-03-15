{-
   File      :  GameConstants.hs
   Represents the constants values that will not change through out the execution of the game.
-}
module GameConstants
(
    screenSize,
    screenWidth,
    screenHeight,
    Size,
    Direction(..) ,
    speed,
    fps,
    calcDistance,
    State(..),
    Frame(..),
    EnemyColor(..),
    Owner(..)
)where

import Graphics.Gloss

type Size = Point

-- sprite motion
data Direction = MoveLeft | MoveRight | MoveUp | NotMoving deriving Show

-- sprite state
data State = Alive | Exploding | Dead deriving (Eq, Show)

-- sprite frame
data Frame = Normal | F1 | F2 | F3 deriving (Eq, Show)

-- enemy color types
data EnemyColor = Red | Blue deriving Show

-- enemy or ship bullet
data Owner = Player | Fighter deriving Show

screenSize :: (Int, Int)
screenSize = (1000,800)

defaultScaling :: Point
defaultScaling = (1.0, 1.0)

screenWidth :: Int
screenWidth = fst screenSize

screenHeight :: Int
screenHeight = snd screenSize

speed :: Float
speed = 30

-- The number of frames per second to render. Typically this is 60 fps
fps:: Int
fps = 60

calcDistance :: Float -> Float
calcDistance deltaTime = deltaTime * speed * fromIntegral(fps)
