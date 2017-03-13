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
    EnemyColor(..)
)where 

import Graphics.Gloss

type Size = Point

data Direction = MoveLeft | MoveRight | MoveUp | NotMoving
data State = Alive | Exploding | Dead
data Frame = Normal | F1 | F2 | F3 deriving Eq
data EnemyColor = Red | Blue

screenSize :: (Int, Int)
screenSize = (1000,800)

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
