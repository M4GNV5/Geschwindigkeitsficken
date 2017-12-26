module Brainfuck.Optimizations.CopyLoops (optimizeLoops) where

import Data.Maybe
import Data.Foldable
import qualified Data.Map.Lazy as M

import Brainfuck

isBasicOp (Add _ (Const _)) = True
isBasicOp (Shift _)         = True
isBasicOp (Comment _)       = True
isBasicOp _                 = False

--TODO optimize ifs and infinite loops
--TODO more loop optimizations when there are Set statements inside or condVal is not -1

analyzeLoop (shift, condOp, mathOps) (Add off (Const val))
    | shift == 0 && off == 0                    = (shift, condOp + val, mathOps)
    | isJust prevOp                             = (shift, condOp, updatedMap)
    | otherwise                                 = (shift, condOp, M.insert target val mathOps)
    where
        target                                  = shift + off
        prevOp                                  = M.lookup target mathOps
        updatedVal                              = val + fromJust prevOp
        updatedMap                              = M.insert target updatedVal mathOps
analyzeLoop (shift, condOp, mathOps) (Shift x)  = (shift + x, condOp, mathOps)
analyzeLoop state (Comment _)                   = state

optimizeLoop loop@(Loop off children)           = if hasNonBasicOps || totalShift /= 0 || condVal /= (-1)
    then [Loop off children']
    else comments ++ optimizedChildren ++ [(Set off (Const 0))]
    where
        children'                               = optimizeLoops children
        comments                                = filter isComment children
        hasNonBasicOps                          = any (not . isBasicOp) children'
        (totalShift, condVal, mathOps)          = foldl analyzeLoop (0, 0, M.empty) children
        optimizedChildren                       = map (\(curr, val) -> Add curr (Var off val)) $ M.toList mathOps

optimizeLoops statements                        = concat $ map optimizeIfLoop statements
    where
        optimizeIfLoop x                        = if isLoop x
            then optimizeLoop x
            else [x]
