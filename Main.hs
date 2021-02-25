module Main where

import           EventHandler
import           GameBoard
import           Physics
import           Rendering

import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Graphics.Gloss.Interface.IO.Game

import           Debug.Trace

-- | Update the game in IO
type Archive = (BreakoutGame, Picture)
updateAIO :: Float -> Main.Archive -> IO Main.Archive
updateAIO s w@(g, p) = do
  game <- updateIO s (fst w)
  return (game, p)

updateIO :: (Float -> BreakoutGame -> IO BreakoutGame)
updateIO s game = return $ update s game

-- | Update the game by moving the ball and bouncing off walls.
update :: Float     -- ^ The number of seconds since last update
       -> BreakoutGame  -- ^ The intial game state
       -> BreakoutGame  -- ^ A new game state with an updated ball and paddles positions.
update seconds =
  movePaddles . wallBounce . paddleBounce . brickBounce . moveBall seconds

-- | Window
window :: Display
window = InWindow "Breakout" (width+500, height) (0, 0)

-- | Background Color
background :: Color
background = black

-- | Frames per second
fps :: Int
fps = 600

-- | Main
-- play :: Display   -- ^ Display window
--      -> Color     -- ^ Background color
--      -> Int       -- ^ Frames per second (FPS)
--      -> BreakoutGame  -- ^ Initial state of the game
--      -> (BreakoutGame -> Picture) -- ^ Rendering the game
--      -> (Event -> BreakoutGame -> BreakoutGame) -- ^ Handlering key events
--      -> (Float -> BreakoutGame -> BreakoutGame) -- ^ Update Game

main :: IO ()
main = do
  bg <- loadBMP "bg.bmp"
  playIO window background fps (initialState, bg) renderIO handleKeysAIO updateAIO
