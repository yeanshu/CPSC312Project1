module GameBoard where

  paddleWidth, paddleHeight, paddleBorder, paddlesDistance, paddleStep, ballRadius, brickwidth, brickheight  :: Float
  paddleWidth = 86
  paddleHeight = 25
  paddleBorder = 1
  paddlesDistance = 0
  paddleStep = 10
  ballRadius = 10
  brickwidth = 80
  brickheight = 30

  width, height, offset:: Int
  width = 1000
  height = 1000
  offset = 450


  type Radius = Float
  type Position = (Float, Float)

  -- | The game state
  data GameState =
    Title | Playing | Paused | Over | Winner
    deriving Show

  -- | A data structure to hold the state of the Breakout game.
  data BreakoutGame = Game
    { gameState :: GameState
    , ballLoc :: (Float, Float) -- ^ Breakout ball (x, y) location.
    , ballVel :: (Float, Float) -- ^ Breakout ball (x, y) velocity.
    , player1 :: Float          -- ^ Paddle x location
                                -- Zero is the middle of the screen.
    , player1v :: Float   -- ^ player1's paddle's velocity.
    , paused :: Bool            -- ^ if the game is paused
    , bricks :: [Bool]      -- list of bricks (if not broken, then true; else if broken, then false)
    , brickloc :: [(Float, Float)] -- list of brick locations
    , score :: Int            -- Current Score
    , speed :: Float          -- Speed of ball
    } deriving Show

  -- | Initialize the game with this game state.
  initialState :: BreakoutGame
  initialState = Game
    { gameState = Title
    , ballLoc = (-100, -100)
    , ballVel = (40, 200)
    , player1 = 0
    , player1v = 0
    , paused  = True
    , bricks = [True | x <- [-3..3], y <- [1..5]]
    , brickloc = [(100*x, 100+50*y) | x <- [-3..3], y <- [1..5]]
    , score = 0
    , speed = 200
    }
