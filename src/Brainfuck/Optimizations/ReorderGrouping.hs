module Brainfuck.Optimizations.ReorderGrouping (reorderAndGroup) where

import Data.List
import Data.Maybe

import Brainfuck

isBlocker off1 stmt          = case stmt of
    Shift _                 -> True --Shifts shoud've been removed by InlineShift
    Loop _                  -> True
    Input off2              -> off1 == off2
    Output off2             -> off1 == off2
    Set _ _ 0 _             -> False
    Set _ off2 _ _          -> off1 == off2
    _                       -> False

assignsTo needle stmt       = case stmt of
    Math off _              -> off == needle
    Set off _ _ _           -> off == needle
    _                       -> False

opsBeforeBlocker off rest   = filter (assignsTo off) $ takeWhile (not . isBlocker off) rest

groupOps stmt1 stmt2        = case stmt2 of
    Math off val2           -> case stmt1 of
        Math _ val1         -> Math off (val1 + val2)
        Set _ src mul val1  -> Set off src mul (val1 + val2)
    Set _ _ _ _             -> stmt2

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
            Math off _      -> Just off
            Set off _ _ _   -> Just off
            _               -> Nothing
        off                 = fromJust offset
        prev                = opsBeforeBlocker off ops
        grouped             = foldl groupOps curr $ opsBeforeBlocker off rest

reorderAndGroup statements  = reverse $ reorderAndGroup' [] statements
