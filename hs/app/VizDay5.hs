module Main (main) where

import Control.Monad (forM_)
import Data.Bits (testBit)
import Data.Bool (bool)
import Numeric (readInt)
import Paths_aoc2020 (getDataFileName)

main :: IO ()
main = do
    names <- lines <$> (readFile =<< getDataFileName "day5.txt")
    let passengers =
          [ (name, (row, col))
          | name <- names
          , (row, rest) <- readInt 2 (`elem` "FB") (fromEnum . (== 'B')) name
          , (col, "") <- readInt 2 (`elem` "LR") (fromEnum . (== 'R')) rest
          ]
        firstSeat@(firstRow, firstCol) = minimum $ snd <$> passengers
        lastSeat = maximum $ snd <$> passengers
        youSeat@(youRow, youCol):_ =
          [ (row, col)
          | row <- [firstRow..]
          , col <- [if row == firstRow then firstCol else 0..7]
          , not $ (row, col) `elem` map snd passengers
          ]
        youName =
            map (bool 'F' 'B' . testBit youRow) [6, 5..0] ++
            map (bool 'L' 'R' . testBit youCol) [2, 1, 0]
        aisleX = firstRow * 8 + 4
    putStrLn "<svg xmlns='http://www.w3.org/2000/svg'"
    putStrLn "    viewBox='0 0 1048 594' width='1048px' height='594px'>"
    putStrLn "  <style>"
    putStrLn "    .name { font-family: monospace; }"
    putStrLn "  </style>"
    putStrLn $ "  <rect x='" ++ show (aisleX - 2) ++ "' y='0' width='12' height='518' fill='#eee'/>"
    putStrLn "  <rect x='0' y='514' width='1048' height='80' rx='8' fill='#ccc'/>"
    forM_ ((,) <$> [0..127] <*> [0..7]) $ \seat@(row, col) -> do
        let x = 8 * if seat < firstSeat then row + 1 else
                if seat > lastSeat then row + 3 else row + 2
            y = 522 + 8 * if col < 4 then 8 - col else 7 - col
        putStrLn $ "  <circle cx='" ++ show x ++ "' cy='" ++ show y ++ "' r='4' fill='#aaa'/>"
    let ts i = fromIntegral i * 0.1 :: Double
        passenger i name (row, col) fill = do
            let t1 = i + 4
                t2 = t1 + row - firstRow + 1
                t3 = t2 + if col < 4 then 4 - col else col - 3
            putStrLn $ "  <g transform='translate(4 550)'>"
            putStrLn "    <animateTransform attributeName='transform' type='translate'"
            putStrLn $ "        from='4 " ++ show (486 - 16 * i) ++
                "' to='4 550' dur='" ++ show (ts t1) ++ "s'/>"
            putStrLn $ "    <circle cx='" ++ show (8 * row + 12) ++
                "' cy='" ++ show (4 + 8 * if col < 4 then 4 - col else 3 - col) ++
                "' r='3' fill='" ++ fill ++ "'>"
            putStrLn $ "    <animate attributeName='cx' values='" ++ show aisleX ++
                ";" ++ show aisleX ++ ";" ++ show (8 * row + 12) ++ "' keyTimes='0;" ++
                show (fromIntegral t1 / fromIntegral t2 :: Double) ++
                ";1' dur='" ++ show (ts t2) ++ "s'/>"
            putStrLn $ "    <animate attributeName='cy' values='4;4;" ++
                show (4 + 8 * if col < 4 then 4 - col else 3 - col) ++ "' keyTimes='0;" ++
                show (fromIntegral t2 / fromIntegral t3 :: Double) ++
                ";1' dur='" ++ show (ts t3) ++ "s'/>"
            putStrLn "    </circle>"
            putStrLn $ "    <text x='" ++ show (aisleX + 10) ++ "' y='8' class='name' opacity='0'>"
            putStrLn $ "      <animate attributeName='opacity' values='1;1;0' keyTimes='0;" ++
                show (fromIntegral (i + 1) / fromIntegral (i + 2) :: Double) ++
                ";1' dur='" ++ show (ts $ i + 2) ++ "s'/>"
            putStrLn $ "      " ++ name
            putStrLn "    </text>"
            putStrLn "  </g>"
    forM_ (zip [32..] passengers) $ \(i, (name, seat)) -> passenger i name seat "#0f0"
    passenger (length passengers + 32) youName youSeat "#f00"
    putStrLn "</svg>"
