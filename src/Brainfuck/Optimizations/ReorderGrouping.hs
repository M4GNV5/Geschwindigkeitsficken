module Brainfuck.Optimizations.ReorderGrouping (reorderAndGroup) where

import Data.List
import Data.Maybe
import Data.Foldable
import qualified Data.Sequence as S

import Brainfuck

getExprSources expr                 = case expr of
    Const _                         -> []
    Var off _                       -> [off]
    Sum _ vars                      -> map fst vars

getSources stmt                     = case stmt of
    Add _ expr                      -> getExprSources expr
    Set _ expr                      -> getExprSources expr
    Output expr                     -> getExprSources expr
    Loop off children               -> off : (concat $ map getSources children)
    _                               -> []

assignsTo off1 stmt                 = case stmt of
    Set off2 _                      -> off1 == off2
    Input off2                      -> off1 == off2
    Loop _ children                 -> any (assignsTo off1) children
    _                               -> False

addsTo off1 stmt                    = case stmt of
    Add off2 _                      -> off1 == off2
    Loop _ children                 -> any (addsTo off1) children
    _                               -> False

changes off1 stmt                   = case stmt of
    Add off2 _                      -> off1 == off2
    Set off2 _                      -> off1 == off2
    Input off2                      -> off1 == off2
    Loop _ children                 -> any (changes off1) children
    _                               -> False

isShift stmt                        = case stmt of
    Shift _                         -> True
    Loop _ children                 -> (not . isZeroShift) children
    _                               -> False

isBlocker off sources stmt          = usesOff || changesOff || changesSource || shifts
    where
        usesOff                     = off `elem` (getSources stmt)
        changesOff                  = changes off stmt
        changesSource               = any (flip changes stmt) sources
        shifts                      = isShift stmt

reorderAndGroup' ops curr
    | isZeroShiftLoop               = (Loop loopOff $ reorderAndGroup children) S.<| ops
    | isNothing off                 = curr S.<| ops
    | isNothing nextUse             = ops S.|> curr
    | canDrop                       = ops
    | canGroup                      = S.update nextUse' grouped ops
    | otherwise                     = S.insertAt nextUse' curr ops
    where
        Loop loopOff children       = curr
        isZeroShiftLoop             = isLoop curr && isZeroShift children
        off                         = case curr of
            Add off _               -> Just off
            Set off _               -> Just off
            _                       -> Nothing
        off'                        = fromJust off
        sources                     = getSources curr
        nextUse                     = S.findIndexL (isBlocker off' sources) ops
        nextUse'                    = fromJust nextUse
        nextUseOp                   = S.index ops nextUse'
        canDrop                     = (not . isLoop) nextUseOp && assignsTo off' nextUseOp
        canGroup                    = (not . isLoop) nextUseOp && addsTo off' nextUseOp
        Add _ nextUseVal            = nextUseOp
        grouped                     = case curr of
            Set _ val               -> Set off' $ addExpressions val nextUseVal
            Add _ val               -> Add off' $ addExpressions val nextUseVal

reorderAndGroup statements          = toList $ foldl reorderAndGroup' S.empty (reverse statements)
