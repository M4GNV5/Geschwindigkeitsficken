module Brainfuck.Optimizations.Grouping (groupStatements) where

import Brainfuck

groupStatement stmts@((Add off1 val1):rest) curr@(Add off2 val2)
    | off1 == off2                              = (Add off1 $ addExpressions val1 val2) : rest
    | otherwise                                 = curr : stmts
groupStatement ((Shift x):rest) (Shift y)       = (Shift $ x + y) : rest
groupStatement ((Comment x):rest) (Comment y)   = (Comment $ x ++ y) : rest
groupStatement rest (Loop off children)         = (Loop off $ groupStatements children) : rest
groupStatement rest curr                        = curr : rest

groupStatements statements                      = reverse $ foldl groupStatement [] statements
