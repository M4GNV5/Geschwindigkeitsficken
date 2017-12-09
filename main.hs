import Data.List
import Data.Maybe
import Data.Foldable
import qualified Data.Sequence as S
import System.Environment

data Statement
    = Math Int              -- *p += arg
    | Set Int               -- *p = arg
    | Copy Int Int Int      -- p[arg0] = *p * arg1 / arg2
    | Shift Int             -- p += arg
    | Loop [Statement]      -- while(*p) { arg }
    deriving(Eq)

isLoop (Loop _) = True
isLoop _        = False

instance Show Statement where
    show (Math x)
        | x < 0         = "*p -= " ++ (show (-x))
        | otherwise     = "*p += " ++ (show x)
    show (Shift x)      = "p += " ++ (show x)
    show (Set x)        = "*p = " ++ (show x)
    show (Copy x y z)   = "p[" ++ (show x) ++ "] = *p" ++ mulStr ++ divStr
        where
            mulStr      = if y == 1
                then ""
                else " * " ++ (show y)
            divStr      = if z == 1
                then ""
                else " / " ++ (show z)
    show (Loop s)       = "while(*p) { " ++ (concat $ map show s) ++ " }"



parseStatements str     = fst $ parseStatements' str

parseStatements' []     = ([], [])
parseStatements' (x:xs)
    | x == ']'          = ([], xs)
    | x == '['          = ((Loop rest) : rest', xs'')
    | otherwise         = (curr : rest, xs')
    where
        curr            = case x of
            '+'         -> Math 1
            '-'         -> Math (-1)
            '>'         -> Shift 1
            '<'         -> Shift (-1)
        (rest, xs')     = parseStatements' xs
        (rest', xs'')   = parseStatements' xs'



groupStatement ((Math x):rest) (Math y)    = (Math $ x + y) : rest
groupStatement ((Shift x):rest) (Shift y)  = (Shift $ x + y) : rest
groupStatement rest curr                   = curr : rest

groupStatements statements                 = reverse $ foldl groupStatement [] statements



analyzeLoop (shift, condOp, mathOps) (Math x)
    | shift == 0                                = (shift, condOp + x, mathOps)
    | isJust prevOp                             = (shift, condOp, updatedSeq)
    | otherwise                                 = (shift, condOp, (shift, x) S.<| mathOps)
    where
        prevOp                                  = S.findIndexL ((==shift) . fst) mathOps
        prevOpIndex                             = fromJust prevOp
        updatedVal                              = x + (snd $ S.index mathOps prevOpIndex)
        updatedSeq                              = S.update prevOpIndex (shift, updatedVal) mathOps
analyzeLoop (shift, condOp, mathOps) (Shift x)  = (shift + x, condOp, mathOps)
analyzeLoop _ x                                 = error $ "analyzeLoop does not support statement " ++ (show x)

copyFromMathOp condVal (offset, value)          = Copy offset value (-condVal)

optimizeLoop loop@(Loop children)               = if hasChildLoops || totalShift /= 0
    then [loop]
    else (map (copyFromMathOp condVal) $ toList mathOps) ++ [(Set 0)]
    where
        children'                               = optimizeLoops children
        hasChildLoops                           = any isLoop children'
        (totalShift, condVal, mathOps)          = foldl analyzeLoop (0, 0, S.empty) children

optimizeLoops statements                        = concat $ map optimizeIfLoop statements
    where
        optimizeIfLoop x                        = if isLoop x
            then optimizeLoop x
            else [x]



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
