module Brainfuck where

import Data.List

data Statement
    = Math Int              -- *p += arg
    | Set Int               -- *p = arg
    | Copy Int Int Int      -- p[arg0] = *p * arg1 / arg2
    | Shift Int             -- p += arg
    | Loop [Statement]      -- while(*p) { arg }
    | Input                 -- *p = getchar()
    | Output                -- putchar(*p)
    deriving(Eq)

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
    show (Loop s)       = "while(*p) { " ++ (intercalate "; " $ map show s) ++ " }"
    show Input          = "*p = getchar()"
    show Output         = "putchar(*p)"

isLoop (Loop _) = True
isLoop _        = False

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
            ','         -> Input
            '.'         -> Output
        (rest, xs')     = parseStatements' xs
        (rest', xs'')   = parseStatements' xs'
