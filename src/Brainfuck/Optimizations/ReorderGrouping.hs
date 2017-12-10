module Brainfuck.Optimizations.ReorderGrouping (reorderAndGroup) where

import Data.List
import Data.Maybe

import Brainfuck

exprUses off1 expr          = case expr of
    Const _                 -> False
    Var _ 0                 -> False
    Var off2 _              -> off1 == off2
    Sum _ vars              -> any (\(off2, mul) -> off1 == off2 && mul /= 0) vars

isBlocker off1 stmt         = case stmt of
    Shift _                 -> True --Shifts shoud've been removed by InlineShift
    Loop _                  -> True
    Input off2              -> off1 == off2
    Output val              -> exprUses off1 val
    Add _ val               -> exprUses off1 val
    Set _ val               -> exprUses off1 val
    _                       -> False

assignsTo needle stmt       = case stmt of
    Add off _               -> off == needle
    Set off _               -> off == needle
    _                       -> False

opsBeforeBlocker off rest   = filter (assignsTo off) $ takeWhile (not . isBlocker off) rest

groupOps stmt1 stmt2        = case stmt2 of
    Add off val2            -> case stmt1 of
        Add _ val1          -> Add off $ addExpressions val1 val2
        Set _ val1          -> Set off $ addExpressions val1 val2
    Set _ _                 -> stmt2

reorderAndGroup' ops []     = ops
reorderAndGroup' ops (curr:rest)
    | isLoop curr           = reorderAndGroup' (optimizedLoop : ops) rest
    | isNothing offset      = reorderAndGroup' (curr : ops) rest
    | null prev             = reorderAndGroup' (grouped : ops) rest
    | otherwise             = reorderAndGroup' ops rest
    where
        Loop children       = curr
        optimizedLoop       = Loop $ reorderAndGroup children
        offset              = case curr of
            Add off _       -> Just off
            Set off _       -> Just off
            _               -> Nothing
        off                 = fromJust offset
        prev                = opsBeforeBlocker off ops
        grouped             = foldl groupOps curr $ opsBeforeBlocker off rest

reorderAndGroup statements  = reverse $ reorderAndGroup' [] statements
