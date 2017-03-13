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

data GameState    = MainMenu 
                  | Game {ship   :: Sprite,
                         enemies :: [Sprite],
                         bullets :: [Sprite],
                         lives   :: [Sprite],
                         score   :: Int
                         }
                  | HighScores 
                  | GameOver 

window :: Display 
window = InWindow "Galaga" screenSize (10,10)

initState :: GameState 
initState = MainMenu 

{- The render function takes a gamestate and renders it to the screen -}
render :: GameState -> IO Picture 
render (Game ship enemies bullets lives score) = do
    ship    <- Ship.render ship
    enemies <- Enemy.render enemies
    unwrapEnemies <- sequence enemies
    bullets       <- Bullet.render bullets
    scoreCounter  <- Scores.scoreCounter score
    lives <- Ship.renderLives lives
    unwrapLives <- sequence lives

    return $ pictures $ [ship] ++ unwrapEnemies ++ unwrapLives ++ [bullets] ++ [scoreCounter]

render (Game ( ship@( Ship {state=Exploding, loc=loc, frame=frame}) ) _ _ _ _) = do
    explosion <- renderExplode loc frame
    return explosion

render HighScores =  Scores.render
render GameOver   =  return.scale 0.5 0.5.translate (-350) (600).color red.text $ "Game Over"   
render MainMenu   =  Menu.render


{- The event handlers handles events coming from the user -}
eventHandler :: Event -> GameState -> IO GameState 
eventHandler (EventKey (Char key) Up _ _) state =  case key of 
        'q' -> return GameOver 
        'p' -> return $ Game mkShip mkInitEnemies [] mkLives 0
        'h' -> return HighScores
        'm' -> return MainMenu
        _  -> return state 

{- This pattern matches for Special key Events-}
eventHandler (EventKey (SpecialKey key) Up _ _) 
  state@(Game ( ship@( Ship {direction=dir}) ) enemies bullets lives score) =
    case key of 
        KeyLeft  ->  return $ (Game  (ship { direction = MoveLeft }) enemies bullets lives score)
        KeyRight ->  return $ (Game  (ship { direction = MoveRight }) enemies bullets lives score)
        keySpace -> return $ (Game ship enemies (addBullet bullets) lives score)
        _        -> return state
        
eventHandler _ state = return state  

{- The game loop for Galaga -}
gameLoop :: Float -> GameState -> IO GameState 
gameloop _ state@(Game ( ship@( Ship {state=Dead}) ) enemies bullets lives score) =
    case lives of
      [] -> return GameOver
      otherwise -> 
          return $ (Game  (ship { state = Alive }) enemies bullets newLives score)
          where
            newLives = drop 1 lives

gameLoop deltaTime state@(Game ship enemies bullets lives score)  = do

    let newShip =  Ship.update deltaTime ship
    let newBullets  = Bullet.update deltaTime (loc ship) bullets       
   
    let newSprites = detectCollision ([newShip] ++ newBullets ++ enemies)

    --is this causing bullet to always exist?
    let newShip2 = newSprites !! 0
    let newBullets2 = [newSprites !! 1]
    let newEnemies = drop 2 newSprites

    -- remove enemies from enemies list after it has exploded for 4 secs
    -- once enemies are removed, compare length enemies - length newEnemies
    -- and that's the new score * 10
    -- == no need to change anything in collision detection
    let newEnemies2 = Enemy.update deltaTime newEnemies  
    -- liveEnemies =  remove dead enemies

    let newScore = updateScore score enemies newEnemies2 -- liveEnemies
    return $ Game newShip2 newEnemies2 newBullets2 lives newScore 

gameLoop _ state  = do
    return state

main :: IO () 
main = playIO window black fps initState Main.render eventHandler gameLoop 
