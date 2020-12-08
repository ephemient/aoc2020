{-# LANGUAGE FlexibleContexts #-}
module Machine (Op(..), parser, step) where

import Data.Functor (($>))
import Data.Vector.Generic (Vector, (!?), fromList)
import Text.Megaparsec (MonadParsec, choice, sepEndBy)
import Text.Megaparsec.Char (newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Op a = Acc a | Jmp a | Nop a deriving (Eq, Ord, Show)

parser :: (Vector v (Op a), Num a, MonadParsec e String m) => m (v (Op a))
parser = fromList <$> parseInstruction `sepEndBy` newline where
    parseInstruction = choice
      [ string "acc" $> Acc
      , string "jmp" $> Jmp
      , string "nop" $> Nop
      ] <* space <*> signed space decimal

step :: (Vector v (Op Int)) => v (Op Int) -> (Int, Int) -> Maybe (Int, Int)
step insns (acc, ip) = evaluate <$> insns !? ip where
    evaluate (Acc x) = acc `seq`(acc + x, ip + 1)
    evaluate (Jmp x) = (acc, ip + x)
    evaluate (Nop _) = (acc, ip + 1)
