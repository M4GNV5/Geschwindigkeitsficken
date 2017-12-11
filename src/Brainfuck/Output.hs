module Brainfuck.Output where

import Data.List

import Brainfuck

--TODO store the first few offsets in registers
ptr                         = "%rdi"
reg1                        = "%al"
reg2                        = "%cl"

constOperand val            = "$" ++ show val
memOperand 0                = "(" ++ ptr ++ ")"
memOperand off              = (show off) ++ "(" ++ ptr ++ ")"

load off reg                = "mov " ++ (memOperand off) ++ ", " ++ reg
loadConst val reg           = "mov " ++ (constOperand val) ++ ", " ++ reg
store reg off               = "mov " ++ reg ++ ", " ++ (memOperand off)
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

cmpConstMem val off         = "cmpb " ++ (constOperand val) ++ ", " ++ (memOperand off)

call func arg
    | arg == reg1           = ["call " ++ func]
    | otherwise             = ["call " ++ func, mov arg reg1]

compileExpression expr      = case expr of
    Const val               -> ("$" ++ show val, [])
    Var off 1               -> (reg1, [load off reg1])
    Var off mul             -> (reg1, [loadConst mul reg1, mulMem off])
    Sum val []              -> ("$" ++ show val, [])
    Sum 0 _                 -> (reg2, compiledVars)
    Sum _ _                 -> (reg2, start : compiledVars)
    where
        Sum val vars        = expr
        start               = loadConst val reg2
        compileVar off 1    = [addMemReg off reg2]
        compileVar off mul  = [loadConst mul reg1, mulMem off, add reg1 reg2]
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
    Input off               -> (loops, strings, call "bfgetchar" (memOperand off) ++ ops)
    Output val              -> (loops, strings, outputExpr val ++ ops)
    Comment str             -> (loops, strings, ("/*" ++ str ++ "*/") : ops)

    Loop children           -> (l, s, ops''')
        where
            loopName        = "loop" ++ (show loops)
            ops'            = ("je " ++ loopName ++ "end") : (cmpConstMem 0 0) : (loopName ++ ":") : ops
            (l, s, ops'')   = foldl compileStatement (loops + 1, strings, ops') children
            ops'''          = (loopName ++ "end:") : ("jmp " ++ loopName) : ops''

    Print str               -> (loops, str : strings, callOp : arg1Op : arg0Op : ops)
        where
            callOp          = "call bfputs"
            arg0Op          = mov ("$str" ++ (show $ length strings)) "%rax"
            arg1Op          = mov (constOperand $ 1 + length str) "%rcx"

compileStatements stmts     = stringsHead ++ stringsBody ++ asmHead ++ asmBody ++ asmTail
    where
        stringsHead         = ".section .rodata\n"
        stringOp (i, str)   = "str" ++ (show i) ++ ":\n\t" ++ ".string " ++ (show str)
        stringsBody         = intercalate "\n" $ map stringOp $ zip [0..] strings
        asmHead             = "\n\n.text\n" ++
            ".global bfmain\n" ++
            ".type bfmain, %function\n" ++
            "bfmain:\n\t"
        asmTail             = "\n\tret"
        asmBody             = intercalate "\n\t" $ reverse ops
        (_, strings, ops)   = foldl compileStatement (0, [], []) stmts
