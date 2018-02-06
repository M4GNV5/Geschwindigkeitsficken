module Brainfuck.Output where

import Data.Char
import Data.Ratio
import Data.List
import Numeric

import Brainfuck

--TODO store the first few offsets in registers
ptr                         = "%rbx"
reg1                        = "%al"
reg2                        = "%cl"
reg3                        = "%ch"

constOperand val            = "$" ++ show val
memOperand 0                = "(" ++ ptr ++ ")"
memOperand off              = (show off) ++ "(" ++ ptr ++ ")"

load off reg                = "movb " ++ (memOperand off) ++ ", " ++ reg
loadConst val reg           = "movb " ++ (constOperand val) ++ ", " ++ reg
store reg off               = "movb " ++ reg ++ ", " ++ (memOperand off)
storeConst val off          = store (constOperand val) off

mov regA regB               = "mov " ++ regA ++ ", " ++ regB

add regA regB               = "add " ++ regA ++ ", " ++ regB
addConst val reg
    | val < 0               = "sub $" ++ (show (-val)) ++ ", " ++ reg
    | otherwise             = "add $" ++ (show val) ++ ", " ++ reg
addConstMem val off         = addConst val (memOperand off)
addMemReg off reg           = "add " ++ (memOperand off) ++ ", " ++ reg
addRegMem reg off           = "add " ++ reg ++ ", " ++ (memOperand off)

mulMem off                  = "mulb " ++ (memOperand off)

load16Z off reg             = "movzx " ++ (memOperand off) ++ ", " ++ reg
divReg reg                  = "divb " ++ reg

cmpConstMem val off         = "cmpb " ++ (constOperand val) ++ ", " ++ (memOperand off)

call func arg
    | arg == "%di"          = ["call " ++ func]
    | otherwise             = ["call " ++ func, mov arg reg1]

compileExpression expr      = case expr of
    Const val               -> (constOperand val, [])
    Var off 1               -> (reg1, [load off reg1])
    Var off mul             -> (reg1, init $ compileVar off mul)
    Sum val []              -> (constOperand val, [])
    Sum _ _                 -> (reg2, start : compiledVars)
    where
        Sum val vars        = expr
        start               = loadConst val reg2
        compileVar off 1    = [addMemReg off reg2]
        compileVar off mul
            | mul == 0      = []
            | mul == 1      = [addMemReg off reg2]
            | denom == 1    = [loadConst numer reg1, mulMem off, add reg1 reg2]
            | numer == 1    = [load16Z off "%ax", loadConst denom reg3, divReg reg3, add reg1 reg2]
            | otherwise     = [loadConst numer reg1, mulMem off, loadConst denom reg3, divReg reg3, add reg1 reg2]
            where
                numer       = numerator mul
                denom       = denominator mul
        compiledVars        = concat $ map (uncurry compileVar) vars

addExprMem off expr         = case expr of
    Const val               -> [addConstMem val off]
    Sum val []              -> [addConstMem val off]
    _                       -> addRegMem reg off : (reverse exprOps)
        where
            (reg, exprOps)  = compileExpression expr

setExprMem off expr         = case expr of
    Const val               -> [storeConst val off]
    Sum val []              -> [storeConst val off]
    _                       -> store reg off : (reverse exprOps)
        where
            (reg, exprOps)  = compileExpression expr

outputExpr expr
    | reg == reg1           = "call bfputchar" : (reverse exprOps)
    | otherwise             = "call bfputchar" : (mov reg reg1) : (reverse exprOps)
    where
        (reg, exprOps)      = compileExpression expr

compileStatement (loops, strings, ops) stmt
                            = case stmt of

    Add off val             -> (loops, strings, addExprMem off val ++ ops)
    Set off val             -> (loops, strings, setExprMem off val ++ ops)
    Shift off               -> (loops, strings, addConst off ptr : ops)
    Input off               -> (loops, strings, store reg1 off : "call bfgetchar" : ops)
    Output val              -> (loops, strings, outputExpr val ++ ops)

    Loop off children       -> (l, s, ops''')
        where
            loopName        = "loop" ++ (show loops)
            ops'            = ("je " ++ loopName ++ "end") : (cmpConstMem 0 off) : (loopName ++ ":") : ops
            (l, s, ops'')   = foldl compileStatement (loops + 1, strings, ops') children
            ops'''          = (loopName ++ "end:") : ("jmp " ++ loopName) : ops''

    Print str               -> (loops, str : strings, callOp : arg1Op : arg0Op : ops)
        where
            callOp          = "call bfputs"
            arg0Op          = mov ("$str" ++ (show $ length strings)) "%rax"
            arg1Op          = mov (constOperand $ 1 + length str) "%rcx"

    Comment str             -> (loops, strings, ("/*" ++ trimmed ++ "*/") : ops)
        where
            trimmed         = dropWhile isSpace $ dropWhileEnd isSpace str

formatString []             = []
formatString (x:xs)
    | x >= ' ' && x <= '~'  = x : formatString xs
    | otherwise             = '\\' : 'x' : showHex (ord x) (formatString xs)

compileStatements stmts     = stringsHead ++ stringsBody ++ asmHead ++ asmBody ++ asmTail
    where
        stringsHead         = ".section .rodata\n"
        stringOp (i, str)   = "str" ++ (show i) ++ ":\n\t" ++ ".string \"" ++ (formatString str) ++ "\""
        stringsBody         = intercalate "\n" $ map stringOp $ zip [0..] strings
        asmHead             = "\n\n.text\n" ++
            ".global bfmain\n" ++
            ".type bfmain, %function\n" ++
            "bfmain:\n\t"
        asmTail             = "\n\tret"
        asmBody             = intercalate "\n\t" $ reverse ops
        (_, strings, ops)   = foldl compileStatement (0, [], []) stmts
