{-
 - I couldn't find a sequence function that worked on signals in
 - Elm's library, so I wrote one myself, which was easy since
 - <~ and ~ are simply Haskell's <$> and <*> for signals. Feel
 - free to add generally useful functions to this file. ~ Silvio
 -}

import Signal(Signal, constant, (~), (<~))

sequence : List (Signal a) -> Signal (List a)
sequence xs = case xs of
                [] -> constant []
                (y :: ys) -> (::) <~ y ~ sequence ys