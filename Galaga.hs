{-
   File      :  Galaga.hs
   Represents the main starting point for the Galaga game.
-}
module Main where

import Ship
import Enemy
import Menu
import Bullet
import Scores
import GameConstants
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Collision
import Sprite

import Debug.Trace

data GameState    = MainMenu
                  | Game {ship   :: Sprite,
                         enemies :: [Sprite],
                         bullets :: [Sprite],
                         lives   :: [Sprite],
                         score   :: Int 
                         }
                  | HighScores
                  | GameOver Int

window :: Display
window = InWindow "Galaga" screenSize (10,10)

initState :: GameState
initState = MainMenu

{- The render function takes a gamestate and renders it to the screen -}
render :: GameState -> IO Picture
render (Game ship enemies bullets lives score) = do
    ship    <- Ship.render ship
    enemies <- Enemy.render enemies
    bullets <- Bullet.render bullets

    unwrapEnemies <- sequence enemies
    unwrapBullets <- sequence bullets

    scoreCounter  <- Scores.scoreCounter score
    lives         <- Ship.renderLives lives
    unwrapLives   <- sequence lives

    return $ pictures $ [ship] ++ unwrapEnemies ++ unwrapLives ++ 
                        unwrapBullets ++ [scoreCounter]


render HighScores =  Scores.renderHighScores
render (GameOver score)   =  Scores.renderGameOver score
render MainMenu  =  Menu.render


{- The event handlers handles events coming from the user -}
eventHandler :: Event -> GameState -> IO GameState
eventHandler (EventKey (Char key) Up _ _) gameState@(Game _ _ _ _ score) = 
    case key of
      'q' -> return $ GameOver score

eventHandler (EventKey (Char key) Up _ _) gameState = case key of
      'q' -> return $ GameOver 0
      'p' -> return $ Game mkShip mkInitEnemies [] mkLives 0
      'h' -> return HighScores
      'm' -> return MainMenu
      _  -> return gameState

{- This pattern matches for Special key Events-}
eventHandler (EventKey (SpecialKey key) Up _ _)
  (Game ( ship@( Ship {direction=dir}) ) enemies bullets lives score) =
    case key of
      KeyLeft  ->  return $ (Game  (ship { direction = MoveLeft }) enemies bullets lives score)
      KeyRight ->  return $ (Game  (ship { direction = MoveRight }) enemies bullets lives score)
      keySpace -> do
        let currShipLoc = loc ship
        return $ Game ship enemies (addShipBullet bullets currShipLoc) lives score

eventHandler _ gameState = return gameState


{- The game loop for Galaga -}
gameLoop :: Float -> GameState -> IO GameState
gameLoop deltaTime gameState@(Game ship enemies bullets lives score) =

  case state ship of 
    Dead -> do 
        let newLives = drop 1 lives
        case newLives of 
            []  -> do
                  saveScore score
                  return $ GameOver score
            otherwise -> 
                return $ Game mkShip enemies bullets newLives score
      
    otherwise -> do
      totalBullets <- fireEnemyBullets enemies bullets

      let newShip    =  Ship.update deltaTime ship
          newBullets = Bullet.update deltaTime totalBullets
          newEnemies = Enemy.update deltaTime enemies

          newSprites = detectCollision ([newShip] ++ newBullets ++ newEnemies)
          (newShip', newBullets', newEnemies') = filterSprites newSprites

          liveEnemies = filterDeadEnemies newEnemies'

          newScore    = updateScore score newEnemies' liveEnemies

          finalEnemies = if (length liveEnemies) < 7
                        then liveEnemies ++ mkInitEnemies
                        else liveEnemies

      return $ Game newShip' finalEnemies newBullets' lives newScore

gameLoop _ gameState  = do
    return gameState

main :: IO ()
main = playIO window black fps initState Main.render eventHandler gameLoop
