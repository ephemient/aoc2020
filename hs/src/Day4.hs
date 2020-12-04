{-|
Module:         Day4
Description:    <https://adventofcode.com/2020/day/4 Day 4: Passport Processing>
-}
{-# LANGUAGE TypeApplications, ViewPatterns #-}
module Day4 (day4a, day4b) where

import Data.Char (isDigit, isHexDigit)
import Data.Ix (inRange)
import Data.List.Split (splitOn)
import Data.Map (Map, (!?), fromList)
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Text.Read (readMaybe)

parseLine :: String -> Map String String
parseLine = fromList . mapMaybe kv . words where
    kv (break (== ':') -> (k, _:v)) = Just (k, v)
    kv _ = Nothing

day4a :: String -> Int
day4a = length . filter ok . map parseLine . splitOn "\n\n" where
    ok m = isJust . mapM_ (m !?) $ words "byr iyr eyr hgt hcl ecl pid"

day4b :: String -> Int
day4b = length . filter ok . map parseLine . splitOn "\n\n" where
    ok m = and
      [ maybe False (inRange (1920, 2002)) $ m !? "byr" >>= readMaybe @Int
      , maybe False (inRange (2010, 2020)) $ m !? "iyr" >>= readMaybe @Int
      , maybe False (inRange (2020, 2030)) $ m !? "eyr" >>= readMaybe @Int
      , case m !? "hgt" >>= listToMaybe . reads @Int of
            Just (n, "cm") -> inRange (150, 193) n
            Just (n, "in") -> inRange (59, 76) n
            _ -> False
      , case m !? "hcl" of
            Just ('#':s) -> length s == 6 && all isHexDigit s
            _ -> False
      , maybe False (`elem` words "amb blu brn gry grn hzl oth") $ m !? "ecl"
      , maybe False (\s -> length s == 9 && all isDigit s) $ m !? "pid"
      ]
