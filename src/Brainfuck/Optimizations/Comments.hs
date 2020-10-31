module Brainfuck.Optimizations.Comments (trimComments, stripComments) where

import Data.Char
import Data.List
import Brainfuck

trim                    = dropWhileEnd isSpace . dropWhile isSpace

trimComment (Comment x) = Comment $ trim x
trimComment x           = x

trimComments stmts      = map trimComment stmts

stripComments []                        = []
stripComments ((Loop cond stmts):rest)  = Loop cond (stripComments stmts) : stripComments rest
stripComments ((Comment _):rest)        = stripComments rest
stripComments (x:xs)                    = x : stripComments xs
