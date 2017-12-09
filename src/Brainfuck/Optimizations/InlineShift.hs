module Brainfuck.Optimizations.InlineShift where

import Brainfuck

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
            Math off val                    -> Math (off + shift) val
            Set off val                     -> Set (off + shift) val
            Copy off1 off2 val add          -> Copy (off1 + shift) (off2 + shift) val add
            Input off                       -> Input (off + shift)
            Output off                      -> Output (off + shift)

inlineShifts statements                     = reverse $ (Shift shift) : ops
    where
        (shift, ops)                        = foldl inlineShifts' (0, []) statements
