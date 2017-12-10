module Brainfuck.Optimizations.InlineShift where

import Brainfuck

shiftExpression shift expr@(Const _)        = expr
shiftExpression shift (Var off val)         = Var (shift + off) val
shiftExpression shift (Sum val vars)        = Sum val $ map (\(off, mul) -> (shift + off, mul)) vars

inlineShifts' (shift, ops) (Shift off)      = (shift + off, ops)
inlineShifts' (shift, ops) op@(Comment _)   = (shift, op : ops)
inlineShifts' (shift, ops) (Loop c)         = if newShift == shift
    then (shift, shiftedLoop : ops)
    else (0, unshiftedLoop : (Shift shift) : ops)
    where
        (newShift, loopOps)                 = foldl inlineShifts' (shift, []) c
        shiftedLoop                         = Loop $ reverse $ loopOps
        unshiftedLoop                       = Loop $ inlineShifts c
inlineShifts' (shift, ops) op               = (shift, newOp : ops)
    where
        newOp                               = case op of
            Add off val                     -> Add (off + shift) $ shiftExpression shift val
            Set off val                     -> Set (off + shift) $ shiftExpression shift val
            Input off                       -> Input (off + shift)
            Output val                      -> Output $ shiftExpression shift val

inlineShifts statements                     = reverse $ (Shift shift) : ops
    where
        (shift, ops)                        = foldl inlineShifts' (0, []) statements
