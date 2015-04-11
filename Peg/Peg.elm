module Peg.Peg where

import Array(Array, get, set, repeat)
import Array
import Maybe(withDefault)
import Maybe
import Util(signum)

type alias Board = Array Bool

initialBoard : Board
initialBoard = set 4 False <| repeat 15 True

coordToIndex : (Int, Int) -> Int
coordToIndex (x, y) = let nthTriangle n = n * (n + 1) // 2
                      in nthTriangle y + x

isValidCoord : (Int, Int) -> Bool
isValidCoord = uncurry (<)

indexToCoord : Int -> (Int, Int)
indexToCoord ind = let nthTriangle n = n * (n + 1) // 2
                       nthTriangleInv m = floor <| sqrt (1/4 + 2.0 * toFloat m) - 1 / 2
                       nearest = nthTriangleInv ind
                   in (ind - nthTriangle nearest, nearest)

length : Board -> Int
length = Array.length

isValidJumpPos : Board -> (Int, Int) -> (Int, Int) -> Bool
isValidJumpPos b (sx, sy) (dx, dy) =
    let cond1 = case (abs <| dx - sx, abs <| dy - sy) of
                  (2, 2) -> signum (dx - sx) == signum (dy - sy)
                  (0, 2) -> True
                  (2, 0) -> True
                  _ -> False
        cond2 = coordToIndex (sx, sy) < length b &&
                coordToIndex (dx, dy) < length b &&
                coordToIndex (sx, sy) >= 0 &&
                coordToIndex (dx, dy) >= 0
        cond3 = sx <= sy && dx <= dy
        cond4 = sx >= 0 && dx >= 0 &&
                sy >= 0 && dy >= 0
    in cond1 && cond2 && cond3 && cond4

canJump : Board -> (Int, Int) -> (Int, Int) -> Bool
canJump b ss dd = let getVal x = withDefault False <| get (coordToIndex x) b
                      mean x y = (x + y) // 2
                      mean2 (x0, y0) (x1, y1) = (mean x0 x1, mean y0 y1)
                  in isValidJumpPos b ss dd &&
                     getVal ss &&
                     getVal (mean2 ss dd) &&
                     not (getVal dd)

doJump : (Int, Int) -> (Int, Int) -> Board -> Board
doJump ss dd b = if not <| canJump b ss dd then
                     b
                 else
                     let mean x y = (x + y) // 2
                         mean2 (x0, y0) (x1, y1) = (mean x0 x1, mean y0 y1)
                         s0 = coordToIndex ss
                         sd = coordToIndex <| mean2 ss dd
                         d0 = coordToIndex dd
                     in b |>
                            set s0 False |>
                            set sd False |>
                            set d0 True