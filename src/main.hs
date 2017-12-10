import Data.List
import Data.Maybe
import System.Environment

import Brainfuck
import Brainfuck.Optimizations.Grouping
import Brainfuck.Optimizations.CopyLoops
import Brainfuck.Optimizations.InlineShift
import Brainfuck.Optimizations.RemoveNoop
import Brainfuck.Optimizations.ReorderGrouping
import Brainfuck.Optimizations.ConstantFold
import Brainfuck.Optimizations.TrailingUnused

optimizations :: [([Statement] -> [Statement])]
optimizations = [
        groupStatements,
        optimizeLoops,
        inlineShifts,
        removeNoops,
        reorderAndGroup,
        constantFold,
        removeTrailing
    ]

validOptions = [
        "i", "o", "code",
        "Onone", "Ogroup", "Ocopyloop", "Oshifts", "Onoop", "Ogroup2", "Oconstfold", "Otrailing"
    ]

parseOptions []             = []
parseOptions (curr:rest)
    | hasTextArg            = (arg, Just textArg) : (parseOptions $ tail rest)
    | otherwise             = (arg, Nothing) : (parseOptions rest)
    where
        arg                 = if head curr == '-'
            then tail curr
            else curr
        hasTextArg          = (not . null) rest && (head $ head rest) /= '-'
        textArg             = head rest

main = do
    args                    <- getArgs
    let options             = parseOptions args
        invalidOptions      = filter (not . (`elem`validOptions) . fst) options
        hasOption str       = isJust $ lookup str options
        forceOption str     = case lookup str options of
            Just opt        -> case opt of
                Just val    -> val
                Nothing     -> error $ "Option -" ++ str ++ " requires a parameter"
            Nothing         -> error $ "Required option -" ++ str ++ " not given"
        getOption def str   = case fromMaybe (Just def) $ lookup str options of
            Just val        -> val
            Nothing         -> error $ "Option -" ++ str ++ " requires a parameter"
        getSwitch def str   = (isJust $ lookup str options) || def

    if null invalidOptions
        then return ()
        else error $ "Invalid options: " ++ (intercalate ", " $ map fst invalidOptions)

    input                   <- if hasOption "i"
        then case getOption undefined "i" of
            "-"             -> getLine
            file            -> readFile file
        else return $ forceOption "code"

    let enableByDefault     = not $ getSwitch False "Onone"
        lxor x y            = (x || y) && (not $ x && y)
        isEnabled str       = enableByDefault `lxor` getSwitch False str
        enabledOs           = [
                isEnabled "Ogroup",
                isEnabled "Ocopyloop",
                isEnabled "Oshifts",
                isEnabled "Onoop",
                isEnabled "Ogroup2",
                isEnabled "Oconstfold",
                isEnabled "Otrailing"
            ]

    let code                = parseStatements input
        optimizations'      = map snd $ filter ((enabledOs!!) . fst) $ zip [0..] optimizations
        optimized           = foldl (flip ($)) code optimizations'

    putStrLn $ intercalate "\n" $ map show optimized
