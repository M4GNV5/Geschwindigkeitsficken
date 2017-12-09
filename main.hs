import Data.List
import Data.Maybe
import qualified Data.Sequence as S
import System.Environment

data Statement
    = Math Int              -- *p += arg
    | Shift Int             -- p += arg
    | Loop [Statement]      -- while(*p) { arg }
    deriving(Show, Eq)

isLoop (Loop _) = True
isLoop _        = False



parseStatements str     = fst $ parseStatements' str

parseStatements' []      = ([], [])
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



optimizations :: [([Statement] -> [Statement])]
optimizations = [groupStatements]

main = do
    args            <- getArgs
    let mode        = args !! 0

    input           <- case mode of
        "stdin"     -> readLn
        "file"      -> readFile $ args !! 1
        "arg"       -> return $ intercalate " " $ tail args

    let code        = parseStatements input
        optimized   = map (flip ($) code) optimizations

    putStrLn $ show optimized
