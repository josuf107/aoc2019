import Intcode

main = do
    result1 <- run <$> readFile "input9.txt"
    print result1
    result2 <- run2 <$> readFile "input9.txt"
    print result2

test1 = initializeProgram [] . parseProgram $ "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
test2 = initializeProgram [] . parseProgram $ "1102,34915192,34915192,7,4,7,99,0"

run = programOutputs . runProgram . initializeProgram [1] . parseProgram
run2 = programOutputs . runProgram . initializeProgram [2] . parseProgram
