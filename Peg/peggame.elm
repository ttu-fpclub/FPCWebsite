{-
 - The traditional "Cracker Barrel" triangle peg game. Currently, the game simply runs.
 - There is no acknowledgement of victory nor is there a way to reset the game or put
 - the starting hole somewhere else. These are all things that I will add soon. I figure
 - this is just a neat little example of FRP that we could put on an examples page
 - somewhere.
 - ~ Silvio
 -}

import Peg.Peg(..)
import Util(sequence, listToMaybe)
import List
import List(concatMap, indexedMap, filter, (::))
import Text(asText)
import Graphics.Collage(..)
import Graphics.Element(Element, flow, down)
import Color(..)
import Signal(..)
import Mouse
import Array
import Array(Array, get, set)
import Maybe

type GameState = Phase1 | Phase2
type Event = NewClick (Maybe Int, Maybe Int) | Restart

coords : List (Float, Float)
coords = [(0.5, 0.0),
          (0.4, 0.19), (0.6, 0.19),
          (0.3, 0.38), (0.5, 0.38), (0.7, 0.38),
          (0.2, 0.57), (0.4, 0.57), (0.6, 0.57), (0.8, 0.57),
          (0.1, 0.76), (0.3, 0.76), (0.5, 0.76), (0.7, 0.76), (0.9, 0.76)]

circleRadius : Float
circleRadius = 10

black : Color
black = rgb 0 0 0

blue : Color
blue = rgb 0 0 255

gray : Color
gray = rgb 128 128 128

circleColor : Int -> Signal Color
circleColor n = let f : ((GameState, Board), (Maybe Int, Maybe Int)) -> Color
                    f ((gs, b), (cNew, cOld)) = let cNew' = indexToCoord <| Maybe.withDefault (-1) <| cNew
                                                    cOld' = indexToCoord <| Maybe.withDefault (-1) <| cOld
                                                in
                                                  if | get n b == Just True -> blue
                                                     | gs == Phase2 && canJump b cNew' (indexToCoord n) -> gray
                                                     | otherwise -> black
                in foldp (\s _ -> f s) (curry f (Phase1, initialBoard) (Nothing, Nothing)) <| map2 (,) currentState mostRecentClicksIndex

singleCircle : Float -> Int -> (Float, Float) -> Signal Form
singleCircle tot n xy = let shape cc = move xy <| filled cc <| circle circleRadius
                        in shape <~ circleColor n

mostRecentClicks : Signal ((Int, Int), (Int, Int))
mostRecentClicks = foldp (\(x1, y1) ((x, y), _) -> ((x1, y1), (x, y))) ((-1, -1), (-1, -1)) <| sampleOn Mouse.clicks Mouse.position

mostRecentClicksIndex : Signal (Maybe Int, Maybe Int)
mostRecentClicksIndex = let distance (x0, y0) (x1, y1) = distance' (x1 - x0, y1 - y0)
                            distance' (x, y) = sqrt (x * x + y * y)
                            f (x, y) =
                                coords
                                    |> List.map2 (,) [0..(List.length coords)]
                                    |> filter (\(_, (x1, y1)) ->
                                        distance (toFloat x, toFloat y)
                                                 (x1 * widthForN gameSize,
                                                  y1 * heightForN gameSize + circleRadius)
                                                     <= circleRadius * 2)
                                    |> List.map fst
                                    |> listToMaybe
                        in (\(c1, c2) -> (f c1, f c2)) <~ mostRecentClicks

restartChannel : Channel ()
restartChannel = channel ()

currentState : Signal (GameState, Board)
currentState = let f : Event -> (GameState, Board) -> (GameState, Board)
                   f ev (gs, b) = case ev of
                                    NewClick (cOld, cNew) ->
                                        case cNew of
                                          Nothing -> (Phase1, b)
                                          Just cNew' -> case gs of
                                                          Phase1 -> case get cNew' b of
                                                                      Nothing -> (Phase1, b)
                                                                      Just False -> (Phase1, b)
                                                                      Just True -> (Phase2, b)
                                                          Phase2 -> case cOld of
                                                                      Nothing -> (Phase1, b)
                                                                      Just cOld' -> case (get cNew' b, get cOld' b) of
                                                                                      (Just False, Just True) -> (Phase1, doJump (indexToCoord cOld') (indexToCoord cNew') b)
                                                                                      _ -> (Phase1, b)
                                    Restart ->
                                        (Phase1, initialBoard)
               in foldp f (Phase1, initialBoard) <| merge (NewClick <~ mostRecentClicksIndex) (always Restart <~ subscribe restartChannel)

widthForN : Float -> Float
widthForN = identity

heightForN : Float -> Float
heightForN n = sqrt 3 * n / 2

game : Float -> Signal Element
game n = let width = widthForN n
             height = heightForN n
             scaledCoords = List.map (\(x, y) -> (x * width, y * height))
                            <| List.map (\(x, y) -> (x - 0.5, 0.5 - y))
                            <| coords
             triangle = constant
                        <| rotate (degrees (-30))
                        <| filled (rgb 255 128 0)
                        <| ngon 3 (n / 2)
             circles = indexedMap (singleCircle n) scaledCoords
             collageList = triangle :: circles
             fullCollage = collage (floor n) (floor n) <~ sequence collageList
         in fullCollage

gameSize : Float
gameSize = 300

main : Signal Element
main = game gameSize