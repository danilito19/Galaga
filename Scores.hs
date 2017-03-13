{- 
   File      :  Scores.hs 
   Save and show scores
-}

module Scores
(
    render,
    scoreCounter,
    updateScore
) where

import Graphics.Gloss
import GameConstants 
import Sprite
import Data.List.Split
import Data.List (intersperse, sort)

scoreCounter :: Int -> IO Picture 
scoreCounter score = return $ scale 0.25 0.25 $ translate (800) (-1300) $ color red (text $ "Current Score: " ++ show score)

-- function to read scores
readScores :: IO [String]--IO [Int]
readScores = do
    contents <- readFile "scores.txt"  
    let strScores = splitOn "\n" contents
    return strScores -- return $ map (read::String->Int) strScores

--function to write scores 
writeScores :: [Int] -> IO ()
writeScores scores = do
    let strScores = listToString scores
    writeFile "scores.txt" strScores

listToString :: [Int] -> String
listToString xs = concat . intersperse "\n" . map show $ xs

-- function to order scores in descending order
sortScores :: [Int] -> [Int]
sortScores xs = reverse $ sort xs

render :: IO Picture 
render = do
    scores <- readScores 
    return $ pictures $ [renderScores scores 0] ++ 
            [scale 0.5 0.5.translate (-350) (600).color white.text $ "High Scores"]

renderScores :: [String] -> Float -> Picture
renderScores [] _ = Blank
renderScores (x:scores) yOffSet = 
    pictures $ [scale 0.5 0.5.translate (0) (yOffSet).color white.text $ x] ++
        [renderScores scores (yOffSet + 200)]

updateScore :: Int -> [Sprite] -> [Sprite] -> Int
updateScore score oldEnemies newEnemies = 10 * ( (length oldEnemies) - (length newEnemies) )
