module Brainfuck.Optimizations.CopyLoops where

import Data.Maybe
import Data.Foldable
import qualified Data.Sequence as S

import Brainfuck

isBasicOp (Loop _)  = False
isBasicOp Input     = False
isBasicOp Output    = False
isBasicOp _         = True

analyzeLoop (shift, condOp, mathOps) (Math x)
    | shift == 0                                = (shift, condOp + x, mathOps)
    | isJust prevOp                             = (shift, condOp, updatedSeq)
    | otherwise                                 = (shift, condOp, (shift, x) S.<| mathOps)
    where
        prevOp                                  = S.findIndexL ((==shift) . fst) mathOps
        prevOpIndex                             = fromJust prevOp
        updatedVal                              = x + (snd $ S.index mathOps prevOpIndex)
        updatedSeq                              = S.update prevOpIndex (shift, updatedVal) mathOps
analyzeLoop (shift, condOp, mathOps) (Shift x)  = (shift + x, condOp, mathOps)
analyzeLoop state (Comment str)                 = state
analyzeLoop _ x                                 = error $ "analyzeLoop does not support statement " ++ (show x)

copyFromMathOp condVal (offset, value)          = Copy offset value (-condVal)

optimizeLoop loop@(Loop children)               = if hasNonBasicOps || totalShift /= 0
    then [loop]
    else (map (copyFromMathOp condVal) $ toList mathOps) ++ [(Set 0)]
    where
        children'                               = optimizeLoops children
        hasNonBasicOps                          = not $ all isBasicOp children'
        (totalShift, condVal, mathOps)          = foldl analyzeLoop (0, 0, S.empty) children

optimizeLoops statements                        = concat $ map optimizeIfLoop statements
    where
        optimizeIfLoop x                        = if isLoop x
            then optimizeLoop x
            else [x]
