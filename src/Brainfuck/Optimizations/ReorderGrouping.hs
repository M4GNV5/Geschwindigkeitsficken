module Brainfuck.Optimizations.ReorderGrouping (reorderAndGroup) where

import Data.List
import Data.Maybe

import Brainfuck

data Blocker = All | Specific Int | None

getBlocker stmt             = case stmt of
    Shift _                 -> All --Shifts shoud've been removed by InlineShift
    Loop _                  -> All
    Input off               -> Specific off
    Output off              -> Specific off
    Copy _ off _ _          -> Specific off
    _                       -> None

isBlocker off stmt          = case getBlocker stmt of
    All                     -> True
    Specific x              -> off == x
    None                    -> False

assignsTo needle stmt       = case stmt of
    Math off _              -> off == needle
    Set off _               -> off == needle
    Copy off _ _ _          -> off == needle
    _                       -> False

opsBeforeBlocker off rest   = filter (assignsTo off) $ takeWhile (not . isBlocker off) rest

groupOps stmt1 stmt2        = case stmt2 of
    Math off val2           -> case stmt1 of
        Math _ val1         -> Math off (val1 + val2)
        Set _ val1          -> Set off (val1 + val2)
        Copy _ src mul val1 -> Copy off src mul (val1 + val2)
    Set _ _                 -> stmt2
    Copy _ _ _ _            -> stmt2

reorderAndGroup' ops []     = ops
reorderAndGroup' ops (curr:rest)
    | isNothing offset      = reorderAndGroup' (curr : ops) rest
    | null prev             = reorderAndGroup' (grouped : ops) rest
    | otherwise             = reorderAndGroup' ops rest
    where
        offset              = case curr of
            Math off _      -> Just off
            Set off _       -> Just off
            Copy off _ _ _  -> Just off
            _               -> Nothing
        off                 = fromJust offset
        prev                = opsBeforeBlocker off ops
        grouped             = foldl groupOps curr $ opsBeforeBlocker off rest

reorderAndGroup statements  = reverse $ reorderAndGroup' [] statements
