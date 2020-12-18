{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies #-}
module Machine (Op(..), parser, step) where

import Data.String (IsString)
import Data.Vector.Generic (Vector, (!?), fromList)
import Text.Megaparsec (MonadParsec, Token, Tokens, choice, sepEndBy)
import Text.Megaparsec.Char (newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Op a = Acc a | Jmp a | Nop a deriving (Eq, Ord, Show)

parser :: (Vector v (Op a), Num a, MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (v (Op a))
parser = fromList <$> parseInstruction `sepEndBy` newline where
    parseInstruction = choice
      [ Acc <$ string "acc"
      , Jmp <$ string "jmp"
      , Nop <$ string "nop"
      ] <* space <*> signed space decimal

step :: (Vector v (Op Int)) => v (Op Int) -> (Int, Int) -> Maybe (Int, Int)
step instructions (acc, ip) = evaluate <$> instructions !? ip where
    evaluate (Acc x) = acc `seq`(acc + x, ip + 1)
    evaluate (Jmp x) = (acc, ip + x)
    evaluate (Nop _) = (acc, ip + 1)
