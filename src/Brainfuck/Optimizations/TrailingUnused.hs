module Brainfuck.Optimizations.TrailingUnused (removeTrailing) where

import Data.List

import Brainfuck

isIOStatement stmt          = case stmt of
    Loop children           -> any isIOStatement children
    Input _                 -> True
    Output _                -> True
    Print _                 -> True
    _                       -> False

removeTrailing statements
    | null ioStatements     = []
    | otherwise             = take usedStatementsCount statements
    where
        ioStatements        = findIndices isIOStatement statements
        usedStatementsCount = 1 + last ioStatements
