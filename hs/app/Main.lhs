# [Advent of Code 2020](https://adventofcode.com/2020)
### my answers in [Haskell](https://www.haskell.org/) ![Haskell CI](https://github.com/ephemient/aoc2020/workflows/Haskell%20CI/badge.svg)

This project builds with [The Haskell Tool Stack](https://haskellstack.org/).

Setup:

```sh
curl -sSL https://get.haskellstack.org/ | sh -s -
stack setup
```

Run the [Hspec](https://hspec.github.io/) test suite:

```sh
stack test aoc2020:test:aoc2020-test
```

Run [criterion](http://www.serpentine.com/criterion/) benchmarks ([results online](https://ephemient.github.io/aoc2020/aoc2020-bench.html)):

```sh
stack bench aoc2020:bench:aoc2020-bench
```

Print solutions for the inputs provided in local data files:

```sh
stack build aoc2020:exe:aoc2020-exe --exec aoc2020-exe
```

Generate [Haddock](https://www.haskell.org/haddock/) API documentation:

```sh
stack haddock aoc2020:lib
```

Run [hlint](https://github.com/ndmitchell/hlint) source code suggestions:

```sh
stack build hlint --exec 'hlint src test bench'
```

---

<!--
```haskell
{-# LANGUAGE NondecreasingIndentation #-}
module Main (main) where
```
-->

## [Day 1: Report Repair](/src/Day1.hs)
```haskell
import Day1 (day1a, day1b)
```

## [Day 2: Password Philosophy](/src/Day2.hs)
```haskell
import Day2 (day2a, day2b)
```

## [Day 3: Toboggan Trajectory](/src/Day3.hs)
```haskell
import Day3 (day3a, day3b)
```

---

```haskell
import Control.Monad ((<=<), when)
import Data.Maybe (mapMaybe)
import Paths_aoc2020 (getDataFileName)
import System.Environment (getArgs)
import Text.Megaparsec (ParseErrorBundle, ShowErrorComponent, Stream, errorBundlePretty)
import Text.Read (readMaybe)

getDayInput :: Int -> IO String
getDayInput i = getDataFileName ("day" ++ show i ++ ".txt") >>= readFile

justOrFail :: (MonadFail m) => Maybe a -> m a
justOrFail = maybe (fail "(⊥)") return

rightOrFail :: (ShowErrorComponent e, Stream s, MonadFail m) =>
    Either (ParseErrorBundle s e) a -> m a
rightOrFail = either (fail . errorBundlePretty) return

run :: Int -> (a -> IO ()) -> [String -> a] -> IO ()
run day showIO funcs = do
    days <- mapMaybe readMaybe <$> getArgs
    when (null days || day `elem` days) $ do
    putStrLn $ "Day " ++ show day
    contents <- getDayInput day
    mapM_ (showIO . ($ contents)) funcs
    putStrLn ""

main :: IO ()
main = do
    run 1 (print <=< justOrFail) [day1a, day1b]
    run 2 (print <=< rightOrFail) [day2a, day2b]
    run 3 print [day3a, day3b]
```
