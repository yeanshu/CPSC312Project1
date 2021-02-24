{-# LANGUAGE NamedFieldPuns #-}

module EventHandler where

  import GameBoard
  import Physics

  import Graphics.Gloss.Interface.Pure.Game
  import System.Exit
  import System.Random

  type Archive = (BreakoutGame,Picture)

  handleKeysAIO :: Event -> Archive -> IO Archive
  handleKeysAIO e w@(g, p) = do 
    game <- handleKeysIO e (fst w)
    return (game, p)

  -- | Respond to key events in IO.
  handleKeysIO :: (Event -> BreakoutGame -> IO BreakoutGame) -- ^ handleKeys function in IO

  -- For an 'q' keypress, exit the game
  handleKeysIO (EventKey (Char 'q') Up _ _) game = exitSuccess

  -- | Just return the same function as handleKeys but in IO
  handleKeysIO event game = return $ handleKeys event game


  -- | Pure responding to key events.
  handleKeys :: Event     -- ^ keyEvent
             -> BreakoutGame  -- ^ Initial game state
             -> BreakoutGame  -- ^ Game updated

  -- For an 'x' keypress on title screen, play level
  handleKeys (EventKey (Char 'x') _ _ _) game@ Game { gameState = Title } = 
    game { gameState = Playing, paused = False}

  -- Easy Difficulty
  handleKeys (EventKey (Char 'z') _ _ _) game@ Game { gameState = Title } = 
    game { 
      gameState = Playing
    , paused = False
    , bricks = [True | x <- [-3,1,3], y <- [1,3,5]]
    , brickloc = [(100*x, 100+50*y) | x <- [-3,0,3], y <- [1,3,5]]
    , speed = 150}

  -- Hard Difficulty
  handleKeys (EventKey (Char 'c') _ _ _) game@ Game { gameState = Title } = 
    game { 
      gameState = Playing
    , paused = False
    , bricks = [True | x <- [-4..4], y <- [1..6]]
    , brickloc = [(100*x, 100+50*y) | x <- [-4..4], y <- [1..6]]
    , speed = 300}

  -- Insane Difficulty
  handleKeys (EventKey (Char 'v') _ _ _) game@ Game { gameState = Title } = 
    game { 
      gameState = Playing
    , paused = False
    , bricks = genBricks--[True | x <- [-4..4], y <- [1..6]]
    , brickloc = zip xs ys--[(100*x, 100+50*y) | x <- [-4..4], y <- [1..6]]
    , speed = 500}
      where
        genBricks = [True | x <- [0..45]]
        xs = [fst (randomR ((-500.0) :: Float, 400.0 :: Float) (mkStdGen x)) | x <- [0..44]]
        ys = [fst (randomR (0.0 :: Float, 400.0 :: Float) (mkStdGen x)) | x <- [45..89]]

  -- For an 'r' keypress, reset the ball to the center.
  handleKeys (EventKey (Char 'r') _ _ _) game@ Game { gameState = Playing } = initialState
  handleKeys (EventKey (Char 'r') _ _ _) game@ Game { gameState = Over } = initialState
  handleKeys (EventKey (Char 'r') _ _ _) game@ Game { gameState = Winner } = initialState

  -- For an 'p' keypress, pause the game.
  handleKeys (EventKey (Char 'p') Up _ _) game@ Game { gameState = Playing } =
    game { gameState = Paused }
  handleKeys (EventKey (Char 'p') Up _ _) game@ Game { gameState = Paused } =
    game { gameState = Playing }

  -- For an 'Left' or 'Right' keypress, move paddle horizontally
  handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game =
    game { player1v = -1 }
  handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game =
    game { player1v = 0 }
  handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game =
    game { player1v = 1 }
  handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game =
    game { player1v = 0 }

  -- Do nothing for all other events.
  handleKeys _ game = game
