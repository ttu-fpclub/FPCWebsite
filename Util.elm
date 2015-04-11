module Util where

{-
 - I couldn't find a sequence function that worked on signals in
 - Elm's library, so I wrote one myself, which was easy since
 - <~ and ~ are simply Haskell's <$> and <*> for signals. Feel
 - free to add generally useful functions to this file. ~ Silvio
 -}

import Signal(Signal, constant, (~), (<~))
import List(..)

sequence : List (Signal a) -> Signal (List a)
sequence xs = case xs of
                [] -> constant []
                (y :: ys) -> (::) <~ y ~ sequence ys

ap : List (a -> b) -> List a -> List b
ap fs xs = concatMap (\f -> map f xs) fs

signum : number -> number
signum x = if | x > 0 -> 1
              | x < 0 -> -1
              | otherwise -> 0

listToMaybe : List a -> Maybe a
listToMaybe xs = case xs of
                   [] -> Nothing
                   (x::_) -> Just x