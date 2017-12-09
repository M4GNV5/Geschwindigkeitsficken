module Brainfuck.Optimizations.Grouping where

import Brainfuck

groupStatement ((Math x):rest) (Math y)    = (Math $ x + y) : rest
groupStatement ((Shift x):rest) (Shift y)  = (Shift $ x + y) : rest
groupStatement rest curr                   = curr : rest

groupStatements statements                 = reverse $ foldl groupStatement [] statements
