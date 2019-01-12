{-# LANGUAGE RecordWildCards #-}

module Lib (
  largestRectangle, naiveSolution
  ) where

import Data.List as List
import Data.Maybe as Maybe

type Stack = [(Int, Int)]

data State = State
  { maxSize :: Int
  , stack :: Stack
  } deriving (Show)

findMax :: Stack -> Int -> Int -> Int -> Int -> (Stack, Int)
findMax [] current index maxSize leftWall = ([(current, 0)], max maxSize ((index + 1) * current))
findMax s@((height, idx):rest) current index maxSize leftWall =
  if height < current
    then ((current, leftWall) : s, maxSize)
    else let newMax = max maxSize $ (index - idx) * height
         in findMax rest current index newMax idx

updateState :: State -> (Int, Int) -> State
updateState s@State {..} (index, current) =
  case stack of
    ((height, _):_) ->
      case compare current height of
        GT -> s {stack = (current, index) : stack}
        EQ -> s
        LT ->
          let (newStack, newMax) = findMax stack current index maxSize 0
          in State {stack = newStack , maxSize = newMax}
    [] -> s {stack = [(current, index)]}

largestRectangle :: [Int] -> Int
largestRectangle xs = maxSize $ List.foldl' updateState (State 0 []) $ zip [0..] (xs ++ [0])

naiveSolution :: [Int] -> Int
naiveSolution [] = 0
naiveSolution xs = List.maximum $ (\x -> List.length x * List.minimum x) <$> subs xs

subs :: [a] -> [[a]]
subs xs = let ts = init $ tails xs
          in List.concatMap (\(h:t) -> (\x -> h : x) <$> inits t) ts
