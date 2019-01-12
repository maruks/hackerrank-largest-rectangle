{-# LANGUAGE RecordWildCards #-}

module Lib (
  largestRectangle, naiveSolution
  ) where

import Data.List as List
import Data.Maybe as Maybe

type Stack = [(Int, Int)]

data State = State
  { maxSize :: Int
  , index :: Int
  , stack :: Stack
  } deriving (Show)

findMax :: Stack -> Int -> Int -> Int -> Int -> (Stack, Int)
findMax [] current index maxSize leftWall = ([(current, 0)], max maxSize ((index + 1) * current))
findMax s@((height, idx):rest) current index maxSize leftWall =
  if height < current
    then ((current, leftWall) : s, maxSize)
    else let newMax = max maxSize $ (index - idx) * height
         in findMax rest current index newMax idx

updateState :: State -> Int -> State
updateState s@State {..} current =
  case stack of
    ((height, _):_) ->
      case compare current height of
        GT -> s { index = index + 1 , stack = (current, index) : stack}
        EQ -> s { index = index + 1}
        LT ->
          let (newStack, newMax) = findMax stack current index maxSize 0
          in State { index = index + 1 , stack = newStack , maxSize = newMax}
    [] -> s { index = index + 1 , stack = [(current, index)]}

largestRectangle :: [Int] -> Int
largestRectangle xs = maxSize $ List.foldl' updateState (State 0 0 []) (xs ++ [0])

naiveSolution :: [Int] -> Int
naiveSolution [] = 0
naiveSolution xs = List.maximum $ (\x -> List.length x * List.minimum x) <$> subs xs

subs :: [a] -> [[a]]
subs xs = let ts = init $ tails xs
          in List.concatMap (\(h:t) -> (\x -> h : x) <$> inits t) ts
