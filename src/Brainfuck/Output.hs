module Brainfuck.Output where

import Data.Maybe
import Data.Char
import Data.Ratio
import Data.List
import Data.Ord
import Numeric

import Brainfuck

ptr                         = "%rbx"
reg1                        = "%al"
reg2                        = "%cl"
reg3                        = "%ch"

--TODO %dl, %dh, %dil, %sil and %r11b are usable too but clobbered when calling bfputs, bfputchar or bfgetchar
usableRegs                  = ["%r8b", "%r9b", "%r10b", "%r12b", "%r13b", "%r14b", "%r15b"]

constOperand val            = "$" ++ show val
memOperand regMap off
    | isJust register       = usableRegs !! fromJust register
    | off == 0              = "(" ++ ptr ++ ")"
    | otherwise             = (show off) ++ "(" ++ ptr ++ ")"
    where
        register            = elemIndex off regMap

loadZero reg                = "xorb " ++ reg ++ ", " ++ reg

load regMap off reg         = "movb " ++ (memOperand regMap off) ++ ", " ++ reg
loadConst val reg           = "movb " ++ (constOperand val) ++ ", " ++ reg
store regMap reg off        = "movb " ++ reg ++ ", " ++ (memOperand regMap off)
storeConst regMap val off   = store regMap (constOperand val) off

mov regA regB               = "mov " ++ regA ++ ", " ++ regB

add regA regB               = "add " ++ regA ++ ", " ++ regB
addConst val reg
    | val < 0               = "sub $" ++ (show (-val)) ++ ", " ++ reg
    | otherwise             = "add $" ++ (show val) ++ ", " ++ reg
addConstMem regMap val off  = addConst val (memOperand regMap off)
addMemReg regMap off reg    = "add " ++ (memOperand regMap off) ++ ", " ++ reg
addRegMem regMap reg off    = "add " ++ reg ++ ", " ++ (memOperand regMap off)

mulMem regMap off           = "mulb " ++ (memOperand regMap off)

loadZX regMap off reg       = "movzx " ++ (memOperand regMap off) ++ ", " ++ reg
divReg reg                  = "divb " ++ reg

cmpConstMem regMap val off  = "cmpb " ++ (constOperand val) ++ ", " ++ (memOperand regMap off)

compileExpression regs expr = case expr of
    Const val               -> (constOperand val, [])
    Var off 1               -> (reg1, [load regs off reg1])
    Var off mul             -> (reg1, init $ compileVar off mul)
    Sum val []              -> (constOperand val, [])
    Sum _ _                 -> (reg2, start : compiledVars)
    where
        Sum val vars        = expr
        start               = loadConst val reg2
        compileVar off 1    = [addMemReg regs off reg2]
        compileVar off mul
            | mul == 0      = []
            | mul == 1      = [addMemReg regs off reg2]
            | denom == 1    = mulOps ++ addOps
            | numer == 1    = loadZX regs off "%ax" : divOps ++ addOps
            | otherwise     = mulOps ++ divOps ++ addOps
            where
                numer       = numerator mul
                denom       = denominator mul
                mulOps      = [loadConst numer reg1, mulMem regs off]
                divOps      = [loadConst denom reg3, divReg reg3]
                addOps      = [add reg1 reg2]
        compiledVars        = concat $ map (uncurry compileVar) vars

addExprMem regs off expr    = case expr of
    Const val               -> [addConstMem regs val off]
    Sum val []              -> [addConstMem regs val off]
    _                       -> addRegMem regs reg off : (reverse exprOps)
        where
            (reg, exprOps)  = compileExpression regs expr

setExprMem regs off expr    = case expr of
    Const val               -> [storeConst regs val off]
    Sum val []              -> [storeConst regs val off]
    _                       -> store regs reg off : (reverse exprOps)
        where
            (reg, exprOps)  = compileExpression regs expr

outputExpr regs expr
    | reg == reg1           = "call bfputchar" : (reverse exprOps)
    | otherwise             = "call bfputchar" : (mov reg reg1) : (reverse exprOps)
    where
        (reg, exprOps)      = compileExpression regs expr

regBlock (Shift _)          = True
regBlock (Loop _ children)  = any regBlock children
regBlock _                  = False

usedOffsets stmt            = case stmt of
    Add off expr            -> ([off], exprUsedOffsets expr)
    Set off expr            -> ([off], exprUsedOffsets expr)
    Input off               -> ([off], [])
    Output expr             -> ([], exprUsedOffsets expr)
    Print _                 -> ([], [])
    Loop off children       -> (childSet, off : childUsed)
        where
            result          = map usedOffsets children
            childSet        = (concat . map fst) result
            childUsed       = (concat . map snd) result
    Comment _               -> ([], [])
    where
        exprUsedOffsets expr= case expr of
            Const _         -> []
            Var off _       -> [off]
            Sum _ vars      -> map fst vars

frequency list              = map (\l -> (length l, head l)) (group (sort list))

regAlloc ops                = if length opsToBlocker < 3
    then []
    else map snd $ take (length usableRegs) offsetUsage
    where
        opsToBlocker        = takeWhile (not . regBlock) ops
        offsets             = map usedOffsets opsToBlocker
        setOffsets          = concat $ map fst offsets
        getOffsets          = concat $ map snd offsets
        highFreq            = filter ((>2) . fst) $ frequency $ setOffsets ++ getOffsets
        offsetUsage         = reverse $ sortBy (comparing fst) highFreq

regMapSwap old new          = (storeOps, loadOps)
    where
        storeOld i off      = store [] (usableRegs !! i) off
        loadNew i off       = load [] off (usableRegs !! i)
        storeOps            = map (uncurry storeOld) $ zip [0..] old
        loadOps             = map (uncurry loadNew) $ zip [0..] new

compileStatement state []           = state
compileStatement (loops, strings, ops, regMap) (stmt:rest)
                                    = compileStatement state' rest
    where
        regMap'                     = regAlloc rest
        (storeOps, loadOps)         = regMapSwap regMap regMap'
        state'                      = case stmt of
            Add off val             -> (loops, strings, addExprMem regMap off val ++ ops, regMap)
            Set off val             -> (loops, strings, setExprMem regMap off val ++ ops, regMap)
            Shift shift             -> (loops, strings, addConst shift ptr : loadOps ++ storeOps ++ ops, regMap')
            Input off               -> (loops, strings, store regMap reg1 off : "call bfgetchar" : ops, regMap)
            Output val              -> (loops, strings, outputExpr regMap val ++ ops, regMap)

            Loop off children
                | regBlocker        -> (loops', strings', opsWithSwap, regMap')
                | otherwise         -> (loops', strings', opsWithoutSwap, regMap)
                where
                    loopName        = "loop" ++ (show loops)
                    loopHeadOps     = ["je " ++ loopName ++ "end", cmpConstMem regMap 0 off, loopName ++ ":"]
                    loopTailOps     = [loopName ++ "end:", "jmp " ++ loopName]
                    regBlocker      = any regBlock children
                    childRegMap     = if regBlocker
                        then regAlloc children
                        else regMap
                    (storeOps, loadChildOps)            = regMapSwap regMap childRegMap
                    (innerStore, loadChildOps')         = regMapSwap innerMap childRegMap
                    (_, loadOps)                        = regMapSwap [] regMap'
                    (loops', strings', ops', innerMap)  = compileStatement (loops + 1, strings, [], childRegMap) children
                    loopOpsWithSwap = loopTailOps ++ loadChildOps' ++ innerStore ++ ops' ++ loopHeadOps
                    opsWithSwap     = loadOps ++ loopOpsWithSwap ++ loadChildOps ++ storeOps ++ ops
                    opsWithoutSwap  = loopTailOps ++ ops' ++ loopHeadOps ++ ops

            Print str               -> (loops, str : strings, callOp : arg1Op : arg0Op : ops, regAlloc rest)
                where
                    callOp          = "call bfputs"
                    arg0Op          = mov ("$str" ++ (show $ length strings)) "%rax"
                    arg1Op          = mov (constOperand $ 1 + length str) "%rcx"

            Comment str             -> (loops, strings, ("/*" ++ trimmed ++ "*/") : ops, regMap)
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
        asmBody             = intercalate "\n\t" $ reverse ops'
        regMap              = regAlloc stmts
        loadOps             = map (loadZero . (usableRegs !!)) $ take (length regMap) [0..]
        (_, strings, ops, _)= compileStatement (0, [], [], regMap) stmts
        ops'                = ops ++ loadOps
