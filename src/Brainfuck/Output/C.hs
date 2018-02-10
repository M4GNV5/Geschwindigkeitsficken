module Brainfuck.Output.C (compileStatements) where

import Data.List
import Data.Ratio
import Control.Monad.State

import Brainfuck

compileExpression expr          = case expr of
    Const x                     -> show x
    Var off mul                 -> showVar off mul
    Sum val vars
        | null vars             -> valStr
        | val == 0              -> varsStr
        | otherwise             -> varsStr ++ " + " ++ valStr
        where
            valStr              = show val
            varsStr             = intercalate " + " $ map (uncurry showVar) vars
    where
        showVar off val
            | val == 1                      = "p[" ++ (show off) ++ "]"
            | denom == 1                    = "p[" ++ (show off) ++ "] * " ++ (show numer)
            | numer == 1                    = "p[" ++ (show off) ++ "] / " ++ (show denom)
            | otherwise                     = "p[" ++ (show off) ++ "] * " ++ (show numer) ++ " / " ++ (show denom)
            where
                numer                       = numerator val
                denom                       = denominator val

compileStatement indent stmt    = (replicate indent '\t') ++ case stmt of
    Add off val                 -> (showOff off) ++ " += " ++ (compileExpression val) ++ ";"
    Set off val                 -> (showOff off) ++ " = " ++ (compileExpression val) ++ ";"
    Shift off
        | off > 0               -> "p += " ++ (show off) ++ ";"
        | off < 0               -> "p -= " ++ (show (-off)) ++ ";"
        | otherwise             -> ""
    Loop off children           -> "while(" ++ (showOff off) ++ ") {\n" ++ body ++ end
        where
            body                = intercalate "\n" $ map (compileStatement (indent + 1)) children
            end                 = "\n" ++ (replicate indent '\t') ++ "}"
    Input off                   -> (showOff off) ++ " = getchar();"
    Output val                  -> "putchar(" ++ (compileExpression val) ++ ");"
    Print str                   -> "printf(\"%s\", " ++ (show str) ++ ");"
    Comment str                 -> "/* " ++ str ++ "*/"
    where
        showOff off             = "p[" ++ (show off) ++ "]"

codeHead                        = "#include <stdio.h>\n" ++
    "#include <stdint.h>\n" ++
    "uint8_t vars[4096] = {0};\n" ++
    "int main() {\n" ++
    "\tuint8_t *p = vars;\n"

codeTail                        = "\n\treturn 0;\n}\n"

compileStatements statements    = codeHead ++ (intercalate "\n" $ map (compileStatement 1) statements) ++ codeTail
