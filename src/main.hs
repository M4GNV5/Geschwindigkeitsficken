import Data.List
import System.Environment

import Brainfuck
import Brainfuck.Optimizations.Grouping
import Brainfuck.Optimizations.CopyLoops

optimizations :: [([Statement] -> [Statement])]
optimizations = [groupStatements, optimizeLoops]

main = do
    args            <- getArgs
    let mode        = args !! 0

    input           <- case mode of
        "stdin"     -> getLine
        "file"      -> readFile $ args !! 1
        "arg"       -> return $ intercalate " " $ tail args

    let code        = parseStatements input
        optimized   = foldl (flip ($)) code optimizations

    putStrLn $ intercalate "\n" $ map show optimized
