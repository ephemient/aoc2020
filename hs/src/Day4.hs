{-|
Module:         Day4
Description:    <https://adventofcode.com/2020/day/4 Day 4: Passport Processing>
-}
{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Day4 (day4a, day4b) where

import Common (readEntire)
import Data.Char (isDigit, isHexDigit)
import Data.Ix (inRange)
import Data.Map (Map)
import qualified Data.Map as Map ((!?), fromList, keysSet)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, isSubsetOf, member)
import Data.Text (Text)
import qualified Data.Text as T (all, breakOn, compareLength, drop, splitOn, stripPrefix, words)
import qualified Data.Text.Read as T (decimal)

parse :: Text -> [Map Text Text]
parse = map (Map.fromList . map (fmap (T.drop 1) . T.breakOn ":") . T.words) .
    T.splitOn "\n\n"

requiredFields, eyeColors :: Set Text
requiredFields = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
eyeColors = Set.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

day4a :: Text -> Int
day4a = length . filter (Set.isSubsetOf requiredFields . Map.keysSet) . parse

day4b :: Text -> Int
day4b = length . filter ok . parse where
    ok m = and
      [ maybe False (inRange (1920, 2002)) $ m Map.!? "byr" >>= readMaybeInt
      , maybe False (inRange (2010, 2020)) $ m Map.!? "iyr" >>= readMaybeInt
      , maybe False (inRange (2020, 2030)) $ m Map.!? "eyr" >>= readMaybeInt
      , case m Map.!? "hgt" >>= either (const Nothing) Just . T.decimal @Int of
            Just (n, "cm") -> inRange (150, 193) n
            Just (n, "in") -> inRange (59, 76) n
            _ -> False
      , maybe False (\s -> hasLength 6 s && T.all isHexDigit s) $
            m Map.!? "hcl" >>= T.stripPrefix "#"
      , maybe False (`Set.member` eyeColors) $ m Map.!? "ecl"
      , maybe False (\s -> hasLength 9 s && T.all isDigit s) $ m Map.!? "pid"
      ]
    readMaybeInt = either (const Nothing) Just . readEntire (T.decimal @Int)
    hasLength n s = T.compareLength s n == EQ
