{-# LANGUAGE NamedFieldPuns #-}

module EventHandler where

  import GameBoard
  import Physics

  import Graphics.Gloss.Interface.Pure.Game
  import System.Exit
  import System.Random

  -- Hack in order to add images to game
  type Archive = (BreakoutGame,Picture)

  handleKeysAIO :: Event -> Archive -> IO Archive
  handleKeysAIO e w@(g, p) = do 
    game <- handleKeysIO e (fst w)
    return (game, p)

  handleKeysIO :: (Event -> BreakoutGame -> IO BreakoutGame) -- ^ handleKeys function in IO

  -- Press Q to quit game
  handleKeysIO (EventKey (Char 'q') Up _ _) game = exitSuccess
  handleKeysIO event game = return $ handleKeys event game


  -- | Pure responding to key events.
  handleKeys :: Event     -- ^ keyEvent
             -> BreakoutGame  -- ^ Initial game state
             -> BreakoutGame  -- ^ Game updated

  -- Press X to Play Game
  handleKeys (EventKey (Char 'x') _ _ _) game@ Game { gameState = Title } = 
    game { gameState = Playing
      , paused = False
      , bricks = [True | x <- [-3..3], y <- [1..5]]
      , brickloc = [(100*x, 100+50*y) | x <- [-3..3], y <- [1..5]]
      , speed = 200}

  -- Press Z for Easy Mode
  -- Fewer Blocks, Slower Ball
  handleKeys (EventKey (Char 'z') _ _ _) game@ Game { gameState = Title } = 
    game { 
      gameState = Playing
    , paused = False
    , bricks = [True | x <- [-3,1,3], y <- [1,3,5]]
    , brickloc = [(100*x, 100+50*y) | x <- [-3,0,3], y <- [1,3,5]]
    , speed = 150}

  -- Press C for Hard Mode
  -- More Blocks, Faster Ball
  handleKeys (EventKey (Char 'c') _ _ _) game@ Game { gameState = Title } = 
    game { 
      gameState = Playing
    , paused = False
    , bricks = [True | x <- [-4..4], y <- [1..6]]
    , brickloc = [(100*x, 100+50*y) | x <- [-4..4], y <- [1..6]]
    , speed = 300}

  -- Press V for Insane Mode
  -- Even Faster Ball, Randomized Blocks
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

  -- Press t for Debug Test Mode
  -- ONLY USED FOR TA DEMO
  handleKeys (EventKey (Char 't') _ _ _) game@ Game { gameState = Title } = 
    game { 
      gameState = Playing
    , paused = False
    , bricks = [True]
    , brickloc = [(0, 100)]
    , speed = 200}

  -- Press b to toggle time limit mode
  handleKeys (EventKey (Char 'b') Down _ _) game@ Game { gameState = Title, timemodeon = False } =
      game {timemodeon = True} 
  handleKeys (EventKey (Char 'b') Down _ _) game@ Game { gameState = Title, timemodeon = True } =
      game {timemodeon = False} 

  -- Press R to go back to title keeping track of fastest victory time
  handleKeys (EventKey (Char 'r') _ _ _) game@ Game { gameState = Playing } = game { gameState = Title
    , ballLoc = (-100, -100)
    , ballVel = (40, 200)
    , player1 = 0
    , player1v = 0
    , paused  = True
    , score = 0
    , time = 0
    , timemodeon = timemodetoggle
    , victorytime = 0
    , fastesttime = fastest
  }
    where
      fastest = fastesttime game --- keeps track of fastest time
      timemodetoggle = timemodeon game

  handleKeys (EventKey (Char 'r') _ _ _) game@ Game { gameState = Over } = game { gameState = Title
    , ballLoc = (-100, -100)
    , ballVel = (40, 200)
    , player1 = 0
    , player1v = 0
    , paused  = True
    , score = 0
    , time = 0
    , timemodeon = timemodetoggle
    , victorytime = 0
    , fastesttime = fastest
  }
    where
      fastest = fastesttime game --- keeps track of fastest time
      timemodetoggle = timemodeon game
      
  handleKeys (EventKey (Char 'r') _ _ _) game@ Game { gameState = Winner } =  game { gameState = Title
    , ballLoc = (-100, -100)
    , ballVel = (40, 200)
    , player1 = 0
    , player1v = 0
    , paused  = True
    , score = 0
    , time = 0
    , timemodeon = timemodetoggle
    , victorytime = 0
    , fastesttime = fastest
  }
    where
      fastest = fastesttime game --- keeps track of fastest time
      timemodetoggle = timemodeon game

  -- Press P to pause game
  handleKeys (EventKey (Char 'p') Up _ _) game@ Game { gameState = Playing } =
    game { gameState = Paused, paused = True}
  handleKeys (EventKey (Char 'p') Up _ _) game@ Game { gameState = Paused } =
    game { gameState = Playing, paused = False}

  -- Moves Player paddle
  -- Key down starts movement, key up ends movement
  handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game =
    game { player1v = -1 }
  handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game =
    game { player1v = 0 }
  handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game =
    game { player1v = 1 }
  handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game =
    game { player1v = 0 }

  -- Nothing
  handleKeys _ game = game
