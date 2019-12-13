module Intcode where

import Data.Foldable
import Data.Maybe
import qualified Data.Map as Map

import Utils

type ProgramMemory = Map.Map Integer Integer

data Program = Program
    { programMemory :: ProgramMemory
    , programPointer :: Integer
    , programInputs :: [Integer]
    , programOutputs :: [Integer]
    , programBase :: Integer
    } deriving(Show)

data Mode = Immediate | Position | Relative deriving (Eq, Ord, Show)
    
withProgramMemory :: With ProgramMemory Program
withProgramMemory f p = p { programMemory = f . programMemory $ p }

withProgramPointer :: With Integer Program
withProgramPointer f p = p { programPointer = f . programPointer $ p }

withProgramInputs :: With [Integer] Program
withProgramInputs f p = p { programInputs = f . programInputs $ p }

withProgramOutputs :: With [Integer] Program
withProgramOutputs f p = p { programOutputs = f . programOutputs $ p }

withProgramBase :: With Integer Program
withProgramBase f p = p { programBase = f . programBase $  p }

testString = runProgram . initializeProgram [] . parseProgram

runProgram :: Program -> Program
runProgram = head . dropWhile (not . endProgram) . iterate stepProgram

endProgram :: Program -> Bool
endProgram program = getValue program (programPointer program) == 99

stepProgram :: Program -> Program
stepProgram program = 
    let
        getImmediate = getOffset program
        getPosition = getValue program . getImmediate
        getRelative = getValue program . (+(programBase program)) . getImmediate
        opCode = parseOpCode . getImmediate $ 0
        modes = parseModes opCode . getImmediate $ 0
        getByMode Position = getPosition
        getByMode Immediate = getImmediate
        getByMode Relative = getRelative
        operands = zipWith (\mode -> getByMode mode) modes [1..]
        operand1 = operands !! 0
        operand2 = operands !! 1
        operand3 = operands !! 2
        inputDestination = if modes !! 0 == Relative then (+(programBase program)) . getImmediate $ 1 else getImmediate 1
        opDestination = if modes !! 2 == Relative then (+(programBase program)) . getImmediate $ 3 else getImmediate 3
        nextInput = head . programInputs $ program
        updatePointer = withProgramPointer (+(opParamCount opCode + 1))
        binaryOp f = withProgramMemory (Map.insert opDestination (f operand1 operand2))
        update = case opCode of
            1 -> updatePointer . binaryOp (+)
            2 -> updatePointer . binaryOp (*)
            3 -> updatePointer . withProgramInputs tail . withProgramMemory (Map.insert inputDestination nextInput)
            4 -> updatePointer . withProgramOutputs (operand1:)
            5 -> if operand1 /= 0 then withProgramPointer (const operand2) else updatePointer
            6 -> if operand1 == 0 then withProgramPointer (const operand2) else updatePointer
            7 -> updatePointer . binaryOp (\x y -> if x < y then 1 else 0)
            8 -> updatePointer . binaryOp (\x y -> if x == y then 1 else 0)
            9 -> updatePointer . withProgramBase (+operand1)
            99 -> id
            x -> error
                $ "Unknown opcode "
                ++ show x
                ++ " in program "
                ++ show program
    in update $ program

opParamCount :: Integer -> Integer
opParamCount = (paramCountMap Map.!)

paramCountMap :: Map.Map Integer Integer
paramCountMap =
    Map.fromList
    . concat
    . fmap (\(ops, count) -> zip ops (repeat count))
    $ [([1, 2, 7, 8], 3), ([3, 4, 9], 1), ([5, 6], 2), ([99], 0)]

parseOpCode :: Integer -> Integer
parseOpCode = read . reverse . take 2 . reverse . show

parseModes :: Integer -> Integer -> [Mode]
parseModes op = fmap (parseMode)
    . rightpad (opParamCount op)
    . fmap (read . pure)
    . drop 2
    . reverse
    . show

parseMode :: Integer -> Mode
parseMode 0 = Position
parseMode 1 = Immediate
parseMode 2 = Relative
parseMode x = error $ "Invalid mode " ++ show x

rightpad :: Integer -> [Integer] -> [Integer]
rightpad n ls = ls ++ replicate (fromIntegral $ n - (fromIntegral $ length ls)) 0

parseProgram :: String -> ProgramMemory
parseProgram = Map.fromList . zip [0..] . fmap read . lines . fmap (\c -> if c == ',' then '\n' else c)

initializeProgram :: [Integer] -> ProgramMemory -> Program
initializeProgram inputs code = Program code 0 inputs [] 0

getValue :: Program -> Integer -> Integer
getValue program i = fromMaybe 0 $ Map.lookup i (programMemory program)

getOffset :: Program -> Integer -> Integer
getOffset program offset = getValue program (programPointer program + offset)

printDebug :: [Integer] -> Program -> String
printDebug watchList program =
    let
        opCode = parseOpCode . flip getOffset 0 $ program
        modes = parseModes opCode . flip getOffset 0 $ program
        tokens = opParamCount opCode + 1
        instruction = take (fromIntegral tokens) . drop (fromIntegral $ programPointer program) . fmap snd . Map.toAscList . programMemory $ program
    in unwords
        [ show . toList $ instruction
        , showOpcode opCode
        , show modes
        , "inputs: " ++ show (programInputs program)
        , "outputs: " ++ show (programOutputs program)
        , "base: " ++ show (programBase program)
        , "watching: " ++ show (fmap (getValue program) watchList)
        ]

showOpcode 1 = "add"
showOpcode 2 = "mult"
showOpcode 3 = "input"
showOpcode 4 = "output"
showOpcode 5 = "jnz"
showOpcode 6 = "jz"
showOpcode 7 = "lt"
showOpcode 8 = "eq"
showOpcode 9 = "rebase"
showOpcode 99 = "end"

runDebug :: [Integer] -> Program -> IO ()
runDebug watchList
    = mapM_ putStrLn
    . fmap (printDebug watchList)
    . (\(p1, p2) -> p1 ++ take 1 p2)
    . span (not . endProgram)
    . iterate stepProgram

nextOutput input program = 
    until (\p -> endProgram p || hasOutput p) stepProgram
    . withProgramOutputs (const [])
    . withProgramInputs (++input)
    $ program

nextInput input program =
    until (\p -> endProgram p || getOffset p 0 == 3) stepProgram
    . withProgramOutputs (const [])
    . withProgramInputs (++input)
    $ program

hasOutput = not . null . programOutputs
