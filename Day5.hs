import Intcode

main :: IO ()
main = do
    result <- runProgram <$> getProgram
    print result
    result2 <- runProgram <$> getProgram2
    print result2

getProgram :: IO Program
getProgram = initializeProgram [1] . parseProgram <$> readFile "input5.txt"

getProgram2 :: IO Program
getProgram2 = initializeProgram [5] . parseProgram <$> readFile "input5.txt"
