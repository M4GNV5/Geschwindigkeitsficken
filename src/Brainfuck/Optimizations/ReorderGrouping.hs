module Brainfuck.Optimizations.ReorderGrouping (reorderAndGroup) where

import Data.List
import Data.Maybe
import Data.Foldable
import qualified Data.Sequence as S

import Brainfuck

shifts stmt                         = case stmt of
    Shift _                         -> True
    Loop _ children                 -> (not . isZeroShift) children
    _                               -> False

isBlocker off sources stmt          = usesOff || changesOff || changesSource || nonZeroShift
    where
        (setOffs, getOffs)          = usedOffsets stmt
        usesOff                     = off `elem` getOffs
        changesOff                  = off `elem` setOffs
        changesSource               = any (`elem`setOffs) sources
        nonZeroShift                = shifts stmt

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
        (_, sources)                = usedOffsets curr
        nextUse                     = S.findIndexL (isBlocker off' sources) ops
        nextUse'                    = fromJust nextUse
        nextUseOp                   = S.index ops nextUse'
        (canDrop, canGroup)         = case nextUseOp of
            Add offN _              -> (False, off' == offN)
            Set offN _              -> (off' == offN, False)
            Input offN              -> (off' == offN, False)
            _                       -> (False, False)
        Add _ nextUseVal            = nextUseOp
        grouped                     = case curr of
            Set _ val               -> Set off' $ addExpressions val nextUseVal
            Add _ val               -> Add off' $ addExpressions val nextUseVal

reorderAndGroup statements          = toList $ foldl reorderAndGroup' S.empty (reverse statements)
