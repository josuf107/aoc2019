import Text.ParserCombinators.ReadP
import Data.Tree

import Graph

main = do
    result <- run <$> readFile "input6.txt"
    print result
    result2 <- run2 <$> readFile "input6.txt"
    print result2

test2 = run2 <$> readFile "test62.txt"

run2 :: String -> Int
run2 input = findSanta . undirectedGraphFromEdges . parseTuples $ input

findSanta g = (subtract 2) . length . takeWhile (notElem "SAN") . levels $ treeFrom g "YOU"

run :: String -> Int
run input = sumDistances . graphFromEdges . parseTuples $ input

sumDistances :: Graph String -> Int
sumDistances g
    = sum
    . fmap (\(level, nodes) -> level * length nodes)
    . zip [0..]
    . levels
    $ treeFrom g "COM"

parseTuples :: String -> [(String, String)]
parseTuples = fmap parseTuple . lines

parseTuple :: String -> (String, String)
parseTuple = fst . head . readP_to_S (do
    part1 <- many1 get
    char ')'
    part2 <- many1 get
    eof
    return (part1, part2))
