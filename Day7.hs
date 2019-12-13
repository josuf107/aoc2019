import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Intcode

main = do
    result <- run <$> readFile "input7.txt"
    print result
    result2 <- runLoops <$> readFile "input7.txt"
    print result2

test1 = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
test2 = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
test3 = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"

run = maximumBy (comparing fst) . tryAllCombos . parseProgram

runLoops = maximumBy (comparing fst) . tryAllLoopCombos . parseProgram

tryAllCombos :: ProgramMemory -> [(Integer, [Integer])]
tryAllCombos program = do
    inputs <- permutations [0..4]
    return (runAmps program inputs, inputs)

tryAllLoopCombos :: ProgramMemory -> [(Integer, [Integer])]
tryAllLoopCombos program = do
    inputs <- permutations [5..9]
    return (runAmpLoops program inputs, inputs)

runAmpLoops :: ProgramMemory -> [Integer] -> Integer
runAmpLoops program phaseSettings =
    getOutput
    . last
    . loopToDone
    . fmap (initializeLoop program)
    $ phaseSettings

initializeLoop program phaseSetting =
    withProgramOutputs (const [0])
    . initializeProgram [phaseSetting]
    $ program

loopToDone :: [Program] -> [Program]
loopToDone programs = case runAmpLoop programs of
    Nothing -> programs
    Just programs' -> loopToDone programs'

runAmpLoop :: [Program] -> Maybe [Program]
runAmpLoop programs =
    let
        runLoopPart ps program =
            let program' = nextOutput [getOutput . last $ ps] program
            in ps ++ [program']
        programs' = drop 1 . foldl runLoopPart [last programs] $ programs
    in
        if hasOutput . head $ programs' then Just programs' else Nothing

getOutput = head . programOutputs

runAmps program phaseSettings = foldl (runAmp program) 0 phaseSettings

runAmp program inputSignal phaseSetting =
    getOutput . runProgram . initializeProgram [phaseSetting, inputSignal] $ program
