module Brainfuck.Optimizations.CopyLoops (optimizeLoops) where

import Data.Maybe
import Data.Foldable
import qualified Data.Sequence as S

import Debug.Trace

import Brainfuck

isBasicOp (Add _ (Const _)) = True
isBasicOp (Shift _)         = True
isBasicOp (Comment _)       = True
isBasicOp _                 = False

analyzeLoop (shift, condOp, mathOps) (Add off (Const val))
    | shift == 0 && off == 0                    = (shift, condOp + val, mathOps)
    | isJust prevOp                             = (shift, condOp, updatedSeq)
    | otherwise                                 = (shift, condOp, (target, val) S.<| mathOps)
    where
        target                                  = shift + off
        prevOp                                  = S.findIndexL ((==target) . fst) mathOps
        prevOpIndex                             = fromJust prevOp
        updatedVal                              = val + (snd $ S.index mathOps prevOpIndex)
        updatedSeq                              = S.update prevOpIndex (target, updatedVal) mathOps
analyzeLoop (shift, condOp, mathOps) (Shift x)  = (shift + x, condOp, mathOps)
analyzeLoop state (Comment _)                   = state

--TODO more loop optimizations when there are Set statements inside or condVal is not -1
optimizeLoop loop@(Loop off children)           = if hasNonBasicOps || totalShift /= 0 || condVal /= (-1)
    then [Loop off children']
    else comments ++ optimizedChildren ++ [(Set 0 (Const 0))]
    where
        children'                               = optimizeLoops children
        comments                                = filter isComment children
        hasNonBasicOps                          = any (not . isBasicOp) children'
        (totalShift, condVal, mathOps)          = foldl analyzeLoop (0, 0, S.empty) children
        optimizedChildren                       = map (\(off, val) -> Add off (Var 0 val)) $ toList mathOps

optimizeLoops statements                        = concat $ map optimizeIfLoop statements
    where
        optimizeIfLoop x                        = if isLoop x
            then optimizeLoop x
            else [x]
