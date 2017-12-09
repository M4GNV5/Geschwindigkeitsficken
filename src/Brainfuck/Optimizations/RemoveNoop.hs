module Brainfuck.Optimizations.RemoveNoop where

import Data.Char

import Brainfuck

isNoop (Math _ 0)           = True
isNoop (Shift 0)            = True
isNoop (Copy _ _ 0 0)       = True
isNoop (Loop [])            = True
isNoop (Comment str)        = all isSpace str
isNoop _                    = False

removeNoops statements      = filter (not . isNoop) statements
