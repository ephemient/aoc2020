{-|
Module:         Day14
Description:    <https://adventofcode.com/2020/day/14 Day 14: Docking Data>
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards #-}
module Day14 (day14a, day14b) where

import Control.Monad (filterM)
import Data.Bits (Bits((.&.), (.|.), clearBit, testBit, xor), FiniteBits(finiteBitSize))
import qualified Data.IntMap as IM
import Data.List (foldl')
import Data.Text (Text)
import Data.Void (Void)
import Data.Word (Word64)
import Numeric (readInt)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, (<|>), oneOf, parse, sepEndBy, some)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Instruction a b c
  = Mask { maskOff :: a, maskOn :: a }
  | Write { writeAddr :: b, writeValue :: c }

parser :: (Num a, Num b, Num c, MonadParsec e Text m) => m [Instruction a b c]
parser = (write <|> mask) `sepEndBy` newline where
    write = Write <$> (string "mem[" *> decimal) <*> (string "] = " *> decimal)
    mask = do
        chars <- string "mask = " *> some (oneOf ['0', '1', 'X'])
        let (maskOff, ""):_ = readInt 2 (const True) (fromEnum . (== '1')) chars
            (maskOn, ""):_ = readInt 2 (const True) (fromEnum . (/= '0')) chars
        pure Mask {..}

day14a :: Text -> Either (ParseErrorBundle Text Void) Word64
day14a input = sum <$> do
    instructions <- parse parser "" input
    pure . fst $ foldl' f (IM.empty, undefined) instructions
  where
    f (mem, _) Mask {..} = (mem, (maskOff, maskOn))
    f (mem, mask@(off, on)) Write {..} =
        (IM.insert writeAddr (writeValue .&. on .|. off) mem, mask)

day14b :: Text -> Either (ParseErrorBundle Text Void) Word64
day14b input = sum <$> do
    instructions <- parse parser "" input
    pure . fst $ foldl' f (IM.empty, (0, 0)) instructions
  where
    f (mem, _) Mask {..} = (mem, (maskOff, maskOn))
    f (mem, mask@(off, on)) Write {..} =
      ( foldl' (flip (`IM.insert` writeValue)) mem $ expand off on writeAddr
      , mask
      )

expand :: (FiniteBits a) => a -> a -> a -> [a]
expand off on x = foldl' clearBit (x .|. on) <$> filterM (const [False, True])
    (filter (testBit $ off `xor` on) [0..finiteBitSize x - 1])
