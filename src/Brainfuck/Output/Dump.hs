module Brainfuck.Output.Dump (dumpStatements) where

import Data.List

import Brainfuck

dumpStatement indent stmt
    | isLoop stmt               = tab ++ loopDump ++ childDump
    | otherwise                 = tab ++ show stmt
    where
        tab                     = replicate indent '\t'
        Loop off children       = stmt
        loopDump                = "Loop " ++ (show off) ++ ":\n"
        childDump               = intercalate "\n" $ map (dumpStatement (indent + 1)) children

dumpStatements statements       = intercalate "\n" $ map (dumpStatement 0) statements
