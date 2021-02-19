module GameBoard where

  paddleWidth, paddleHeight, paddleBorder, paddlesY, paddleStep, ballRadius, brickwidth, brickheight  :: Float
  paddleWidth = 86
  paddleHeight = 25
  paddleBorder = 1
  paddlesY = (-200)
  paddleStep = 5
  ballRadius = 10
  brickheight = 10
  brickwidth = 50

  width, height, offset:: Int
  width = 1000
  height = 1000
  offset = 450


  type Radius = Float
  type Position = (Float, Float)

  -- | The game state
  data GameState =
    Playing | Paused | Over
    deriving Show

  -- | A data structure to hold the state of the Breakout game.
  data BreakoutGame = Game
    { gameState :: GameState
    , ballLoc :: (Float, Float) -- ^ Breakout ball (x, y) location.
    , ballVel :: (Float, Float) -- ^ Breakout ball (x, y) velocity.
    , player1 :: Float          -- ^ Left player paddle height.
                                -- Zero is the middle of the screen.
    , player1v :: Float   -- ^ player1's paddle's velocity.
    , paused :: Bool            -- ^ if the game is paused
    , over :: Bool            -- ^ If the game is over
    } deriving Show

  -- | Initialize the game with this game state.
  initialState :: BreakoutGame
  initialState = Game
    { gameState = Playing
    , ballLoc = (0, 100)
    , ballVel = (40, -140)
    , player1 = 0
    , player1v = 0
    , paused  = False
    , over = False
    }
