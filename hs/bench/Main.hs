module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, env, nf)
import Day1 (day1a, day1b)
import Paths_aoc2020 (getDataFileName)

getDayInput :: Int -> IO String
getDayInput i = getDataFileName ("day" ++ show i ++ ".txt") >>= readFile

main :: IO ()
main = defaultMain
  [ env (getDayInput 1) $ \input -> bgroup "Day 1"
      [ bench "part 1" $ nf day1a input
      , bench "part 2" $ nf day1b input
      ]
  ]
