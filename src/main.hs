import Data.List
import Data.Maybe
import System.IO
import System.Exit
import System.Process
import System.FilePath
import System.Directory
import System.Environment

import Brainfuck
import qualified Brainfuck.Output.X64Assembly as ASM
import qualified Brainfuck.Output.C as C
import Brainfuck.Output.Dump
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

compilers :: [(String, [Statement] -> String)]
compilers = [
        (".dump", dumpStatements),
        (".S", ASM.compileStatements),
        (".c", C.compileStatements)
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

assembleAndLink asm out     = do
    tmpDir                  <- getTemporaryDirectory
    (path, fd)              <- openTempFile tmpDir "bf.S"

    selfDir                 <- takeDirectory <$> getExecutablePath
    let enviroment          = selfDir ++ "/environment.o"

    hPutStr fd asm
    hPutChar fd '\n'
    hFlush fd

    asExit                  <- system $ "as " ++ path ++ " -o " ++ path ++ ".o"
    case asExit of
        ExitSuccess         -> return ()
        ExitFailure code    -> do
            removeFile path
            error $ "as exited with code " ++ (show code)

    ldExit                  <- system $ "ld " ++ path ++ ".o " ++ enviroment ++ " -o " ++ out
    case ldExit of
        ExitSuccess         -> return ()
        ExitFailure code    -> do
            removeFile path
            removeFile (path ++ ".o")
            error $ "ld exited with code " ++ (show code)

    return ()

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
        outFile             = getOption "a.out" "o"

    case lookup (takeExtension outFile) compilers of
        Just compiler       -> writeFile outFile (compiler optimized)
        Nothing             -> assembleAndLink (ASM.compileStatements optimized) outFile
