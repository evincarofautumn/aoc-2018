# Advent of Code 2018

`AOC.hs` contains all solutions, written in Haskell. `main` prints the results
of each question on a single line, with multi-part answers in tuples. My
challenge to myself was to write these solutions completely in point-free style,
relying heavily on combinators from `Control.Arrow` and `Control.Applicative` to
do so—I use the left-to-right Arrow composition operator `>>>` instead of the
typical right-to-left function composition operator `(.)`. I don’t advise
writing real Haskell code like this, but I’m publishing it in the hope that it
teaches you some interesting idioms that could help improve your code…when used
sparingly.
