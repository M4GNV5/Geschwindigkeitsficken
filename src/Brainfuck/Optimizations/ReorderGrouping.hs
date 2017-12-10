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
    _                               -> []

assigntsTo off1 stmt                = case stmt of
    Set off2 _                      -> off1 == off2
    Input off2                      -> off1 == off2
    Loop children                   -> (not . isZeroShift) children || any (assigntsTo off1) children
    _                               -> False

isAddTo off1 stmt                   = case stmt of
    Add off2 _                      -> off1 == off2
    _                               -> False

usesOffset off stmt                 = off `elem` (getSources stmt) || assigntsTo off stmt || isAddTo off stmt

reorderAndGroup' ops curr
    | isNothing off                 = curr S.<| ops
    | isNothing nextUse             = ops
    | assigntsTo off' nextUseOp     = ops
    | isAddTo off' nextUseOp        = S.update nextUse' grouped ops
    | otherwise                     = S.insertAt nextUse' curr ops
    where
        off                         = case curr of
            Add off _               -> Just off
            Set off _               -> Just off
            _                       -> Nothing
        off'                        = fromJust off
        nextUse                     = S.findIndexL (usesOffset off') ops
        nextUse'                    = fromJust nextUse
        nextUseOp                   = S.index ops nextUse'
        Add _ nextUseVal            = nextUseOp
        grouped                     = case curr of
            Set _ val               -> Set off' $ addExpressions val nextUseVal
            Add _ val               -> Add off' $ addExpressions val nextUseVal


reorderAndGroup statements          = toList $ foldl reorderAndGroup' S.empty (reverse statements)
