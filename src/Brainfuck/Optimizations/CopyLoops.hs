module Brainfuck.Optimizations.CopyLoops where

import Data.Maybe
import Data.Foldable
import qualified Data.Sequence as S

import Brainfuck

isBasicOp (Math _ _)    = True
isBasicOp (Shift _)     = True
isBasicOp (Comment _)   = True
isBasicOp _             = False

analyzeLoop (shift, condOp, mathOps) (Math off val)
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
analyzeLoop state (Comment str)                 = state

copyFromMathOp condVal (offset, value)          = Copy offset value 1

optimizeLoop loop@(Loop children)               = if hasNonBasicOps || totalShift /= 0 || condVal /= (-1)
    then [loop]
    else comments ++ optimizedChildren ++ [(Set 0 0)]
    where
        comments                                = filter isComment children
        children'                               = optimizeLoops children
        hasNonBasicOps                          = not $ all isBasicOp children'
        (totalShift, condVal, mathOps)          = foldl analyzeLoop (0, 0, S.empty) children
        optimizedChildren                       = map (copyFromMathOp condVal) $ toList mathOps

optimizeLoops statements                        = concat $ map optimizeIfLoop statements
    where
        optimizeIfLoop x                        = if isLoop x
            then optimizeLoop x
            else [x]
