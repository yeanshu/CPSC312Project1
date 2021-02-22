{-# LANGUAGE NamedFieldPuns #-}

module EventHandler where

  import GameBoard
  import Physics

  import Graphics.Gloss.Interface.Pure.Game
  import System.Exit

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

  -- For an 'r' keypress, reset the ball to the center.
  handleKeys (EventKey (Char 'r') _ _ _) game@ Game { gameState = Playing } =
    game { ballLoc = (0, 0), ballVel = (40, -140) }
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
