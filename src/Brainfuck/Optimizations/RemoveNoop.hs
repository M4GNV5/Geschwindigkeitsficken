module Brainfuck.Optimizations.RemoveNoop (removeNoops) where

import Data.Char

import Brainfuck

isZero (Const 0)            = True
isZero (Var _ 0)            = True
isZero (Sum 0 vars)         = all ((==0) . snd) vars
isZero _                    = False

isNoop (Add _ val)          = isZero val
isNoop (Shift 0)            = True
isNoop (Comment str)        = all isSpace str
isNoop _                    = False

removeNoops statements      = map removeNoops' $ filter (not . isNoop) statements
    where
        removeNoops' x      = case x of
            Loop off c      -> Loop off $ removeNoops c
            _               -> x
