{- 
   File      :  Scores.hs 
   Save and show scores
-}

module Scores
(
    renderHighScores,
    scoreCounter,
    updateScore,
    renderGameOver,
    saveScore
) where

import Graphics.Gloss
import GameConstants 
import Sprite
import Data.List.Split
import Data.List (intersperse, sort)
import Control.Monad (when)


-- render the score during the game
scoreCounter :: Int -> IO Picture 
scoreCounter score = return $ scale 0.25 0.25 $ translate (800) (-1000) $ color red (text $ "Current Score: " ++ show score)

-- save a player's score to a file along with existing scores
saveScore :: Int -> IO ()
saveScore score = do
    oldScores <- readScores 
    let intOldScores = map (\x -> read x :: Int) oldScores
        allScores = intOldScores ++ [score]
        finalScores = sortScoresTop10 allScores
    when (length finalScores > 0) $
        writeScores finalScores

-- function to read scores
readScores :: IO [String]
readScores = do
    contents <- readFile "scores.txt"  
    let strScores = splitOn "\n" contents
    return strScores 

--function to write scores 
writeScores :: [Int] -> IO ()
writeScores scores = do
    let strScores = listToString scores
    writeFile "scores.txt" strScores

-- convert a list of ints to a string
listToString :: [Int] -> String
listToString xs = concat . intersperse "\n" . map show $ xs

-- function to order scores in descending order and return top 10
sortScoresTop10 :: [Int] -> [Int]
sortScoresTop10 xs =  take 10 $ reverse $ sort xs

-- render the high scores picture
renderHighScores :: IO Picture 
renderHighScores = do
    scores <- readScores 
    let revScores = reverse scores
    return $ pictures $ [renderScores revScores 100] ++ 
            [scale 0.5 0.5.translate (-350) (600).color white.text $ "High Scores"]

-- render game over screen with score
renderGameOver :: Int -> IO Picture 
renderGameOver score = do 
    return( pictures 
                [scale 0.5 0.5.translate (-350) (400).color red.text $ "Game Over",
                scale 0.25 0.25.translate (-350) (200).color white.text $ "Your Score",
                 scale 0.5 0.5 $ color white (text $ show score)
                 ] 
            )
-- render a list of scores
renderScores :: [String] -> Float -> Picture
renderScores [] _ = Blank
renderScores (x:scores) yOffSet = 
    pictures $ [scale 0.5 0.5.translate (0) (yOffSet).color white.text $ x] ++
        [renderScores scores (yOffSet + 100)]

-- update a player's score by comparing the number of killed enemies * 10
updateScore :: Int -> [Sprite] -> [Sprite] -> Int
updateScore score oldEnemies newEnemies = score + (10 * ( (length oldEnemies) - (length newEnemies) ))
