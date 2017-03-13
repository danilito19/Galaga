module Main where 

import Graphics.Gloss

newtype Ship  = Ship Point 
newtype Enemy = Enemy Point  

data GameState = GameState Ship Enemy 

window :: Display 
window = InWindow "Game Example" (800,600) (10,10)

main 	
 = display 
        (InWindow
        "Galaga" 	 -- window title
        (400, 150) 	 -- window size -- use from constants
        (10, 10)) 	 -- window position
    green			 -- background color
    picture			 -- picture to display


picture	
    = Translate (-170) (-20)   -- shift the text to the middle of the window
    $ Scale 0.5 0.5		       -- display it half the original size
    $ Text "Hello World"	   -- text to display
