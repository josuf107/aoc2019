import qualified Data.Map as Map

import Intcode

main = do
    program <- initializeProgram [] . parseProgram <$> readFile "input2.txt"
    print (runProgram . adjust 12 2 $ program)
    let (noun, verb) = findNounVerb program
    print (100 * noun + verb)

test = do
    program <- parseProgram <$> readFile "test2.txt"
    print (runProgram . initializeProgram [] $ program)

adjust :: Integer -> Integer -> Program -> Program
adjust noun verb = withProgramMemory (Map.insert 1 noun . Map.insert 2 verb)

findNounVerb :: Program -> (Integer, Integer)
findNounVerb program
    = head
    . filter (\(noun, verb) -> (==19690720) . flip getValue 0 . runProgram . adjust noun verb $ program)
    $ [(x, y) | x <- [0..99], y <- [0..99]]
