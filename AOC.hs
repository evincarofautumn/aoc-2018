{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Applicative
import Control.Arrow ((&&&), (***), (>>>), (|||), arr)
import Control.Monad ((>=>), join)
import Data.Bool (bool)
import Data.Function (fix)
import Data.List (tails)
import Data.List (group, sort)
import Data.Maybe (mapMaybe)
import Text.Parsec (digit, eof, parse, space, string)
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

main :: IO ()
main
  = day day1 "input1.txt"
  *> day day2 "input2.txt"
  *> day day3 "input3.txt"
  where
    day = (>>> pure) >>> (readFile >=>) >>> (>=> print)

day1 :: String -> (Int, Int)
day1
  = lines
  >>> map parseDelta
  >>> sum &&& (cycle >>> partialSums >>> firstDuplicate)
  where
    partialSums = scanl (+) 0
    parseDelta = dropWhile (== '+') >>> read
    firstDuplicate = fix checkNext mempty
      where
        checkNext = curry $ curry $ bool
          <$> (loop <*> (IntSet.insert <$> x <*> acc) <*> xs)
          <*> x
          <*> (IntSet.member <$> x <*> acc)
          where
            x = head <$> snd
            xs = tail <$> snd
            acc = fst >>> snd
            loop = fst >>> fst

day2 :: String -> (Int, String)
day2
  = lines
  >>> (map histogram
      >>> filter (any (count >>> (== 2))) &&& filter (any (count >>> (== 3)))
      >>> both length
      >>> multiply)
    &&& (upperTriangle
      >>> concat
      >>> map (fst &&& diff)
      >>> filter (countDifferences >>> (== 1))
      >>> head
      >>> uncurry (zip >>> fmap extractCommon))
  where
    histogram = sort >>> group >>> map (head &&& length)
    count = snd
    multiply = arr (uncurry (*))
    both = join (***)

    upperTriangle = zipWith ((,) >>> fmap) <*> tails
    diff = uncurry (zipWith (/=))
    countDifferences = snd >>> map fromEnum >>> sum
    extractCommon = mapMaybe (uncurry (Just >>> flip bool Nothing))

day3 :: String -> Int
day3 = parse
    (many ((,)
      <$> (string "#" *> int <* space)
      <*> ((,)
        <$> ((,)
          <$> (string "@" *> space *> int)
          <*> (string "," *> int))
        <*> ((,)
          <$> (string ":" *> space *> int)
          <*> (string "x" *> int <* space)))) <* eof)
    "input"
  >>> (show >>> error)
  ||| (map dimensions
    >>> concatMap (liftA2 (,) <$> range x <*> range y)
    >>> countPoints
    >>> Map.filter (>= 2)
    >>> Map.size)
  where
    int = read @Int <$> some digit

    _claimId = fst
    dimensions = snd
    position = fst
    size = snd
    x = fst
    y = snd
    countPoints = map (pairWith (1 :: Int)) >>> Map.fromListWith (+)
    pairWith = flip (,)

    range = liftA2 enumFromTo
      <$> (position >>>)
      <*> (fmap pred <$> (liftA2 (+) <$> (position >>>) <*> (size >>>)))
