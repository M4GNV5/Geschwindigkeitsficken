module Brainfuck.Optimizations.CopyLoops (optimizeLoops) where

import Data.Ratio
import Data.Maybe
import Data.Foldable
import qualified Data.Map.Lazy as M

import Brainfuck

isBasicOp (Add _ (Const _)) = True
isBasicOp (Shift _)         = True
isBasicOp (Comment _)       = True
isBasicOp _                 = False

--TODO optimize ifs and infinite loops
--TODO can we optimize when condVal < 0?

{-
the simple copy loop is
[->++>++++<<]
or
while p[0] != 0:
    p[0] -= 1
    p[1] += 2
    p[2] += 4

we can simplify this to
p[1] += p[0] * 2
p[2] += p[0] * 4
p[0] = 0

the general form is
p[1] += p[0] * x_1
...
p[n] += p[0] * x_n
p[0] = 0
which only works when p[0] gets decremented by 1

when p[0] is changed other than -1 we need the special AdditionsUntilZero statement
it adds x_0 to p[0] until it reaches zero and returns the count of additions (note that x_0 might be <0)
p[0] = AdditionsUntilZero(p[0], x_0)
p[1] += p[0] * x_1
...
p[n] += p[0] * x_n
p[0] = 0
-}

analyzeLoop (shift, condOp, mathOps) (Add off (Const val))
    | target == 0                               = (shift, condOp + val, mathOps)
    | isJust prevOp                             = (shift, condOp, updatedMap)
    | otherwise                                 = (shift, condOp, M.insert target val mathOps)
    where
        target                                  = shift + off
        prevOp                                  = M.lookup target mathOps
        updatedVal                              = val + fromJust prevOp
        updatedMap                              = M.insert target updatedVal mathOps
analyzeLoop (shift, condOp, mathOps) (Shift x)  = (shift + x, condOp, mathOps)
analyzeLoop state (Comment _)                   = state

optimizeLoop loop@(Loop off children)           = if hasNonBasicOps || totalShift /= 0 || condVal == 0
    then [Loop off children']
    else if condVal == -1
        then comments ++ optimizedChildren ++ [(Set off (Const 0))]
        else comments ++ (AddUntilZero off condVal : optimizedChildren) ++ [Set off (Const 0)]
    where
        children'                               = optimizeLoops children
        comments                                = filter isComment children
        hasNonBasicOps                          = any (not . isBasicOp) children'
        (totalShift, condVal, mathOps)          = foldl analyzeLoop (0, 0, M.empty) children
        optimizedChildren                       = map genMathOp $ M.toList mathOps
        condVal'                                = if condVal == -1
            then negate condVal
            else 1
        genMathOp (curr, val)                   = Add curr (Var off (val % condVal'))

optimizeLoops statements                        = concat $ map optimizeIfLoop statements
    where
        optimizeIfLoop x                        = if isLoop x
            then optimizeLoop x
            else [x]
