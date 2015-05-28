import Graphics.Element exposing (..)

import Markdown
main : Element
main = Markdown.toElement """
  # Welcome to the FPC Page
  * Projects
    - [This Website](https://github.com/ttu-fpclub/fpc-website/)
    - [A Chemistry Aid](https://github.com/thepygoscelis/chemistryapp)
    - More to come soon.. - Charlie
  * Languages
    - [Racket](http://racket-lang.org/)
    - [R](http://www.r-project.org/)
    - [Elm](http://elm-lang.org/)
    - [Haskell](https://www.haskell.org/)
    - [Common Lisp](https://common-lisp.net/)
    - [Scheme](http://www.scheme.com/)
    - [Elixir](http://elixir-lang.org/)
    - [Erlang](http://www.erlang.org/)
  """
