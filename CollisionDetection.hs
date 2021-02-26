module CollisionDetection where

  import GameBoard

  -- Return True if the ball has collided with the paddle
  paddleCollision :: BreakoutGame -> Bool
  paddleCollision game =
    ((deltaXP1 * deltaXP1 + deltaYP1 * deltaYP1) <
      (ballRadius * ballRadius)) 
    where
      -- Ball's center
      (ballX, ballY) = ballLoc game
      -- Player 1's paddle's center
      recXP1 = player1 game
      recYP1 = -200
      -- Player A's paddle's left bottom corner (needed for collision math)
      rectCornerXP1 = recXP1 -paddleWidth / 2
      rectCornerYP1 = recYP1 -paddleHeight / 2

      deltaXP1 = ballX - max rectCornerXP1 (min ballX (rectCornerXP1 + paddleWidth))
      deltaYP1 = ballY - max rectCornerYP1 (min ballY (rectCornerYP1 + paddleHeight))

  -- Return true if ball collides with top or bottom wall given ball position and radius
  wallCollisionTB :: Position -> Radius -> Bool
  wallCollisionTB (_, y) radius =
      topCollision || bottomCollision 
    where
      topCollision = y + radius >= fromIntegral width / 2
      bottomCollision = y - radius <= -(fromIntegral width / 2)

   -- Return true if ball collides with left or right wall given ball position and radius
  wallCollisionLR :: Position -> Radius -> Bool
  wallCollisionLR (x, _) radius =  rightCollision || leftCollision
    where
      rightCollision = x + radius >= fromIntegral height / 2
      leftCollision = x - radius <= -(fromIntegral height / 2)

  -- Returns a pair (Bool, [Bool]). fst pair indicates if there is a brickCollision in one of the bricks
  -- snd pair is used to find which brick was collided
  brickCollision :: BreakoutGame -> (Bool,[Bool])
  brickCollision game = isTrue
    where
      (ballX, ballY) = ballLoc game
      locationList = brickloc game

      -- apply math formula from paddleCollision to every brick
      locationListCornerLB = map (\(x,y) -> (x-brickwidth/2, y-brickheight/2)) locationList
      locationListDelta = map (\(x,y) -> (ballX - max x (min ballX (x+brickwidth)), ballY - max y (min ballY (y+brickheight)))) locationListCornerLB
      isTrueList = [x*x + y*y < (ballRadius*ballRadius) | (x,y) <- locationListDelta]

      -- if one brick is collided, should return true
      truth = or isTrueList
      isTrue = (truth,isTrueList)

  -- Depending on where the ball hits the brick, return an int which signifies how the ball velocity will change in Physics.hs
  --    1) if ball (coming from left side) hits bottom-left corner to corner + 1/3 width of brick (in x) or
  --       if ball (coming from right side) hits bottom-right corner to corner - 1/3 width of brick (in x) or
  --       if ball hits corner + 1/3 height of brick (in y)
  --            vx,vy --> -vx,-vy
  --    2) if ball hits corner + 1/3 height of brick to top of the brick (in y)
  --            vx,vy --> -vx, vy
  --    3) if ball (coming from left side) hits bottom-left corner + 1/3 width of brick to bottom-right corner (in x) or
  --       if ball (coming from right side) hits bottom-right corner - 1/3 width of brick to bottom-left corner (in x)
  --            vx,vy --> vx, -vy
  brickCollisionConditions :: Float -> (Float, Float) -> (Float, Float) -> Int
  brickCollisionConditions velx (cx,cy) (brickx, bricky)
    | cy >= brickLBCornerY && cy <= brickLBCornerY + brickheight/3                              = 1
    | cy >= brickLBCornerY + brickheight/3 && cy <= brickLBCornerY + brickheight                = 2
    | velx > 0 && cx >= brickLBCornerX && cx <= brickLBCornerX + brickwidth/3                   = 1
    | velx < 0 && cx <= brickLBCornerX + brickwidth && cx >= brickLBCornerX + 2*brickwidth/3    = 1
    | otherwise                                                                                 = 3
      where
        brickLBCornerX = brickx - brickwidth/2
        brickLBCornerY = bricky - brickheight/2
    