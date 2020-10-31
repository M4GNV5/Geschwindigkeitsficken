module Brainfuck.Optimizations.Comments (trimComments, stripComments, splitCommentsOnNewline) where

import Data.Char
import Data.List
import Brainfuck

trim                    = dropWhileEnd isSpace . dropWhile isSpace

trimComment (Comment x) = Comment $ trim x
trimComment (Loop o xs) = Loop o $ trimComments xs
trimComment x           = x

trimComments stmts      = map trimComment stmts


splitCommentsOnNewline []               = []
splitCommentsOnNewline ((Comment x):xs) = newComments ++ xs'
    where
        newComments                     = map Comment $ filter ((>0) . length) $ map trim $ lines x
        xs'                             = splitCommentsOnNewline xs
splitCommentsOnNewline ((Loop o ls):xs) = (Loop o $ splitCommentsOnNewline ls) : splitCommentsOnNewline xs
splitCommentsOnNewline (x:xs)           = x : splitCommentsOnNewline xs


stripComments []                        = []
stripComments ((Loop cond stmts):rest)  = Loop cond (stripComments stmts) : stripComments rest
stripComments ((Comment _):rest)        = stripComments rest
stripComments (x:xs)                    = x : stripComments xs
