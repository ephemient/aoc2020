{-|
Module:         Day14
Description:    <https://adventofcode.com/2020/day/14 Day 14: Docking Data>
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies, RecordWildCards #-}
module Day14 (day14a, day14b) where

import Data.Bits (Bits((.&.), (.|.), shiftL, shiftR, popCount, xor))
import qualified Data.IntMap as IntMap (empty, insert)
import Data.List (foldl')
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Data.Word (Word64)
import Numeric (readInt)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, (<|>), oneOf, parse, sepEndBy, some)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Instruction a b c
  = Mask { maskOff :: a, maskOn :: a }
  | Write { writeAddr :: b, writeValue :: c }

parser :: (Num a, Num b, Num c, MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m [Instruction a b c]
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
    pure . fst $ foldl' f (IntMap.empty, undefined) instructions
  where
    f (mem, _) Mask {..} = (mem, (maskOff, maskOn))
    f (mem, mask@(off, on)) Write {..} =
        (IntMap.insert writeAddr (writeValue .&. on .|. off) mem, mask)

day14b :: Text -> Either (ParseErrorBundle Text Void) Word64
day14b input = sum <$> do
    instructions <- parse parser "" input
    pure . fst $ foldl' f (IntMap.empty, (0, 0)) instructions
  where
    f (mem, _) Mask {..} = (mem, (maskOff, maskOn))
    f (mem, mask@(off, on)) Write {..} =
      ( foldl' (flip (`IntMap.insert` writeValue)) mem $
            xor (writeAddr .|. off) <$> bitPowerSet (off `xor` on)
      , mask
      )

bitPowerSet :: (Bits a, Enum a, Num a) => a -> [a]
bitPowerSet bits = chooseBits <$> [0..1 `shiftL` popCount bits - 1] where
    chooseBits i = fst $ foldl' (f i) (0, bits) [0..popCount bits - 1]
    f i (x, k) j =
        (x `xor` bits .&. (k `xor` (k - i `shiftR` j .&. 1)), k .&. (k - 1))
