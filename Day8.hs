import Data.Char
import Data.Function
import Data.List
import Data.Ord

import qualified Data.Map as Map

main = do
    result <- run <$> readFile "input8.txt"
    print result
    result2 <- run2 <$> readFile "input8.txt"
    len <- length <$> readFile "input8.txt"
    print len
    putStrLn result2

run2
    = printGridMap
    . foldl combineLayers (Map.fromList $ zip (makePoints height width) (repeat 2))
    . reverse
    . fmap (Map.fromList . zip (makePoints height width))
    . parse

printGridMap :: Map.Map (Int, Int) Int -> String
printGridMap gridMap = unlines . fmap (concat . fmap snd) . groupBy ((==) `on` (fst . fst)) . Map.toAscList . fmap showPoint $ gridMap

showPoint :: Int -> String
showPoint 1 = "B"
showPoint _ = " "

combineLayers :: Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
combineLayers back front = Map.mapWithKey (\point value -> combinePoints (front Map.! point) value) back

combinePoints :: Int -> Int -> Int
combinePoints front back =
    case (front, back) of
        (1, _) -> 1
        (0, _) -> 0
        (2, x) -> x

makePoints :: Int -> Int -> [(Int, Int)]
makePoints rows columns = concat $ fmap (\row -> fmap (\column -> (row, column)) [0..columns - 1]) [0..rows - 1]

run = (\layer -> (length . filter (==1) $ layer) * (length . filter (==2) $ layer))
    . minimumBy (comparing (length . filter (==0)))
    . parse

parse = chunkify (width * height)
    . fmap (read . pure)
    . filter isDigit

chunkify :: Int -> [a] -> [[a]]
chunkify _ [] = []
chunkify n list = let (chunk, remainder) = splitAt n list in chunk:(chunkify n remainder)

width = 25

height = 6
