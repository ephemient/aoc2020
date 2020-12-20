{-|
Module:         Day20
Description:    <https://adventofcode.com/2020/day/20 Day 20: Jurrasic Jigsaw>
-}
{-# LANGUAGE FlexibleContexts, NamedFieldPuns, NondecreasingIndentation, OverloadedStrings, RecordWildCards, TypeApplications, TypeFamilies #-}
module Day20 (day20a, day20b) where

import Control.Applicative (Alternative((<|>), empty))
import Control.Arrow ((***))
import Data.Bits (Bits((.|.), clearBit, setBit, shiftL, testBit), FiniteBits(finiteBitSize))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap ((!), fromList, fromListWith, keysSet, toList)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet (delete, intersection, null, singleton, toList)
import Data.List (foldl', inits, transpose)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (fromList, head, init, last, tail)
import Data.Set (Set)
import qualified Data.Set as Set (fromDistinctAscList, isSubsetOf, mapMonotonic, size, unions)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, between, choice, count, count', parse, sepEndBy, skipSome)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Tile = Tile
  { tileSize :: Int
  , tileBits :: [[Bool]]
  , tileBorderTop :: Int
  , tileBorderLeft :: Int
  , tileBorderBottom :: Int
  , tileBorderRight :: Int
  }

transforms :: [[a]] -> [[[a]]]
transforms xss = [id, transpose] <*>
    map (foldr ($) xss) (inits [reverse, transpose, reverse])

tileTransforms :: Tile -> [Tile]
tileTransforms tile = [id, transposeTile] <*>
    map (foldr ($) tile) (inits [flipTile, transposeTile, flipTile])

flipTile, transposeTile :: Tile -> Tile
flipTile tile = tile
  { tileBits = reverse $ tileBits tile
  , tileBorderTop = tileBorderBottom tile
  , tileBorderLeft = reverseBits (tileSize tile) $ tileBorderLeft tile
  , tileBorderRight = reverseBits (tileSize tile) $ tileBorderRight tile
  , tileBorderBottom = tileBorderTop tile
  }
transposeTile tile = tile
  { tileBits = transpose $ tileBits tile
  , tileBorderTop = tileBorderLeft tile
  , tileBorderLeft = tileBorderTop tile
  , tileBorderRight = tileBorderBottom tile
  , tileBorderBottom = tileBorderRight tile
  }

reverseBits :: (Bits a) => Int -> a -> a
reverseBits size a = foldl' f a [0..size `div` 2 - 1] where
    f a' i
      | a' `testBit` i == a' `testBit` j = a'
      | a' `testBit` i = a' `clearBit` i `setBit` j
      | otherwise = a' `setBit` i `clearBit` j
      where j = size - 1 - i

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (IntMap Tile)
parser = IntMap.fromList <$> parseEntry `sepEndBy` skipSome newline where
    parseEntry = (,) <$> parseTitle <*> parseTile
    parseTitle = between (string "Tile ") (char ':' *> newline) decimal
    parseTile = do
        row <- (:|) <$> parseBit <*>
            count' 1 (finiteBitSize @Int 0 - 1) parseBit
        let tileSize = length row
        bitmap <- (row:|) <$> count (tileSize - 1)
            ((:|) <$ newline <*> parseBit <*> count (tileSize - 1) parseBit)
        pure Tile
          { tileSize
          , tileBits = stripEnds <$> stripEnds bitmap
          , tileBorderTop = makeBorder $ NonEmpty.head bitmap
          , tileBorderLeft = makeBorder $ NonEmpty.head <$> bitmap
          , tileBorderBottom = makeBorder $ NonEmpty.last bitmap
          , tileBorderRight = makeBorder $ NonEmpty.last <$> bitmap
          }
    parseBit = False <$ char '.' <|> True <$ char '#'
    makeBorder = foldl' f 0 where f acc bit = acc `shiftL` 1 .|. fromEnum bit
    stripEnds = NonEmpty.init . NonEmpty.fromList . NonEmpty.tail

assemble :: (Alternative f) => IntMap Tile -> f [NonEmpty (Int, Tile)]
assemble tiles = growDown (IntMap.keysSet tiles) Nothing where
    borders = IntMap.fromListWith (<>)
      [ (border, IntSet.singleton tileId)
      | (tileId, Tile {..}) <- IntMap.toList tiles
      , border <- [id, reverseBits tileSize] <*>
          [ tileBorderTop
          , tileBorderLeft
          , tileBorderBottom
          , tileBorderRight
          ]
      ]

    growDown :: (Alternative f) => IntSet -> Maybe (NonEmpty Int) -> f [NonEmpty (Int, Tile)]
    growDown ids _ | IntSet.null ids = pure []
    growDown ids above = choice
      [ (row' :) <$> growDown ids' (Just $ tileBorderBottom . snd <$> row')
      | chosenId <- IntSet.toList $ case above of
            Just (above' :| _) ->
                IntSet.intersection ids $ borders IntMap.! above'
            _ -> ids
      , chosenTile <- tileTransforms $ tiles IntMap.! chosenId
      , maybe True ((== tileBorderTop chosenTile) . NonEmpty.head) above
      , (ids', row) <- growRight (IntSet.delete chosenId ids)
            (NonEmpty.tail <$> above) (tileBorderRight chosenTile)
      , let row' = (chosenId, chosenTile) :| row
      ]

    growRight :: (Alternative f) => IntSet -> Maybe [Int] -> Int -> f (IntSet, [(Int, Tile)])
    growRight ids (Just []) _ = pure (ids, [])
    growRight ids above left = choice
      [ case above of
            Nothing -> fmap ((chosenId, chosenTile):) <$>
                growRight ids'' Nothing (tileBorderRight chosenTile)
            Just (_:above') -> fmap ((chosenId, chosenTile):) <$>
                growRight ids'' (Just above') (tileBorderRight chosenTile)
            _ -> pure (ids'', [(chosenId, chosenTile)])
      | let ids' = IntSet.intersection ids $ borders IntMap.! left
      , chosenId <- IntSet.toList $ maybe ids'
            (IntSet.intersection ids' . (borders IntMap.!) . head) above
      , chosenTile <- tileTransforms $ tiles IntMap.! chosenId
      , left == tileBorderLeft chosenTile &&
            maybe True ((== tileBorderTop chosenTile) . head) above
      , let ids'' = IntSet.delete chosenId ids
      ] <|> maybe (pure (ids, [])) (const empty) above

day20a :: Text -> Either (ParseErrorBundle Text Void) (Maybe Int)
day20a input = do
    tiles <- parse parser "" input
    pure $ do
    image <- nonEmpty =<< assemble tiles
    pure $ fst (NonEmpty.head $ NonEmpty.head image) *
        fst (NonEmpty.last $ NonEmpty.head image) *
        fst (NonEmpty.head $ NonEmpty.last image) *
        fst (NonEmpty.last $ NonEmpty.last image)

day20b :: Text -> Either (ParseErrorBundle Text Void) (Maybe Int)
day20b input = do
    tiles <- parse parser "" input
    pure $ do
    image <- nonEmpty =<< assemble tiles
    let assembledTiles = fmap (tileBits . snd) <$> image
    let bitmap = foldr1 (++) $ foldr1 (zipWith (++)) <$> assembledTiles
        points = bitmapToSet bitmap
        monsterPoints = Set.unions
          [ monster
          | y <- [0..length bitmap - 1]
          , x <- [0..maximum (length <$> bitmap) - 1]
          , monster <- Set.mapMonotonic ((+ y) *** (+ x)) <$> monsters
          , monster `Set.isSubsetOf` points
          ]
    pure $ Set.size points - Set.size monsterPoints

bitmapToSet :: [[Bool]] -> Set (Int, Int)
bitmapToSet bitmap = Set.fromDistinctAscList
    [(y, x) | (y, row) <- zip [0..] bitmap, (x, True) <- zip [0..] row]

monsters :: [Set (Int, Int)]
monsters = map bitmapToSet . transforms $ map (== '#') <$>
  [ "                  # "
  , "#    ##    ##    ###"
  , " #  #  #  #  #  #   "
  ]
