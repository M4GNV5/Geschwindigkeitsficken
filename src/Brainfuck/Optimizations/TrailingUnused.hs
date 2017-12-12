module Brainfuck.Optimizations.TrailingUnused (removeTrailing) where

import Data.List

import Brainfuck

isIOStatement stmt          = case stmt of
    Loop _ children         -> any isIOStatement children
    Input _                 -> True
    Output _                -> True
    Print _                 -> True
    _                       -> False

removeTrailing statements   = dropWhileEnd (not . isIOStatement) statements
