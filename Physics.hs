{-# LANGUAGE NamedFieldPuns #-}

module Physics where

  import GameBoard
  import CollisionDetection

  import Data.Maybe


  -- | Move both paddles depending on the velocity
  movePaddles :: BreakoutGame -- ^ Initial game state
              -> BreakoutGame -- ^ new game with updated paddle position
  movePaddles game = game { player1 = movePaddle paddleStep (player1v game) (player1 game)
                          }

  -- | Update the paddle position
  movePaddle :: Float     -- ^ The step
             -> Float     -- ^ Paddle's velocity
             -> Float     -- ^ The initial player state
             -> Float     -- ^ The new player state with an updated paddle position


  movePaddle step velocity player

        -- ^ No step , no mouvement
        | velocity == 0 = player
        -- ^ Below ceiling, but trying to go down.
        | player >= fromIntegral offset && velocity < 0 =
            player + (step *  velocity)
        -- ^ Under floor, but trying to go up.
        | player <= fromIntegral (-offset) && velocity > 0 =
            player + (step *  velocity)
        -- ^Between the two walls.
        | player > fromIntegral (-offset) && player < fromIntegral offset =
            player + (step *  velocity)
        | otherwise = player

  -- | Ball Universe

  -- | Update the ball position using its current velocity.
  moveBall :: Float     -- ^ The number of seconds since last Update
           -> BreakoutGame  -- ^ The initial game state
           -> BreakoutGame  -- ^ A new game state with an updated ball position


  -- When paused, don't move.
  moveBall _ game@ Game { paused } | paused = game

  -- Moving the ball.
  moveBall seconds game =
    game { ballLoc = (x' , y') }
    where
      -- Old locations and velocities
      (x, y) = ballLoc game
      (vx, vy) = ballVel game

      --New locations
      x' = x + vx * seconds
      y' = y + vy * seconds


  -- | Detect a collision with a paddle. Upon collisions,
  -- change the velocity of the ball to bounce it off the paddle.
  paddleBounce :: BreakoutGame  -- ^ The initial game state
               -> BreakoutGame  -- ^ A new game state with an updated ball velocity

  paddleBounce game = game { ballVel = (vx, vy') }
    where
        (vx, vy) = ballVel game

        vy' = if paddleCollision game
              then
                  -- Update the velocity
                  (-vy)

                  else
                  -- Do nothing.Return te old velocity
                  vy

  -- | Detect a collision with one of the side walls. Upon collisions,
  -- update the velocity of the ball to bounce it off the wall.
  wallBounce :: BreakoutGame  -- ^ The initial game state
             -> BreakoutGame  -- ^ A new game state with an updated ball velocity

  wallBounce game = game { ballVel = (vx', vy') }
    where
        -- Radius. Use the same thing as in `render`.

        -- The old Velocities.
        (vx, vy) = ballVel game

        vy' = if wallCollisionTB (ballLoc game) ballRadius
              then
                  -- Update the velocity
                  (-vy)
                  -- -vy
                  else
                  -- Do nothing.Return te old velocity
                  vy
        vx' = if wallCollisionLR (ballLoc game) ballRadius then (-vx) else vx
