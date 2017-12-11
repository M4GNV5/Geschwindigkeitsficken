module Brainfuck.Optimizations.InlineShift (inlineShifts) where

import Brainfuck

shiftExpression shift expr@(Const _)        = expr
shiftExpression shift (Var off val)         = Var (shift + off) val
shiftExpression shift (Sum val vars)        = Sum val $ map (\(off, mul) -> (shift + off, mul)) vars

inlineShifts' (shift, ops) (Shift off)      = (shift + off, ops)
inlineShifts' (shift, ops) (Loop off c)     = if isZeroShift c
    then (shift, shiftedLoop : ops)
    else (0, unshiftedLoop : (Shift shift) : ops)
    where
        (newShift, loopOps)                 = foldl inlineShifts' (shift, []) c
        shiftedLoop                         = Loop (off + shift) $ reverse $ loopOps
        unshiftedLoop                       = Loop off c
inlineShifts' (shift, ops) op               = (shift, newOp : ops)
    where
        newOp                               = case op of
            Add off val                     -> Add (off + shift) $ shiftExpression shift val
            Set off val                     -> Set (off + shift) $ shiftExpression shift val
            Input off                       -> Input (off + shift)
            Output val                      -> Output $ shiftExpression shift val
            _                               -> op

inlineShifts statements                     = reverse $ (Shift shift) : ops
    where
        (shift, ops)                        = foldl inlineShifts' (0, []) statements
