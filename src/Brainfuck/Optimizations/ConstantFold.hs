module Brainfuck.Optimizations.ConstantFold where

import Data.Char
import Data.Maybe
import Data.Traversable
import Data.Map.Lazy as M

import Brainfuck

valuesToOps values                  = valueOps
    where
        valueToOp off (Just val)    = Set off 0 0 val
        valueOpMap                  = M.mapWithKey valueToOp $ M.filter isJust values
        valueOps                    = Prelude.map snd $ toList valueOpMap

isPrint (Print _)   = True
isPrint _           = False

constantFold' (True, values, ops) curr                  = (True, values, curr : ops)
constantFold' (False, values, ops) curr                 = case curr of
    Math off val1                           -> case getValue off of
        Nothing                             -> (False, values, curr : ops)
        Just val2                           -> (False, setValue off (Just $ val1 + val2), ops)
    Set off _ 0 val                         -> (False, setValue off (Just val), ops)
    Set off1 off2 mul add                   -> case getValue off2 of
        Nothing                             -> (False, setValue off1 Nothing, ops)
        Just val                            -> (False, setValue off1 (Just $ val * mul + add), ops)
    Shift _                                 -> (True, values, curr : valueOps ++ ops)
    Loop _                                  -> (True, values, curr : valueOps ++ ops)
    Input off                               -> (False, setValue off Nothing, ops)
    Output off                              -> case getValue off of
        Nothing                             -> (False, values, curr : ops)
        Just val                            -> case firstOp of
            Just (Print str)                -> (False, values, (Print $ str ++ [chr val]) : tail ops)
            Nothing                         -> (False, values, (Print [chr val]) : ops)
    Comment _                               -> (False, values, curr : ops)
    where
        setValue off val                    = M.insert off val values
        getValue off                        = fromMaybe (Just 0) $ M.lookup off values
        valueOps                            = valuesToOps values
        firstOp                             = if Prelude.null ops
            then Nothing
            else Just $ head ops

constantFold statements                     = reverse $ valueOps ++ ops
    where
        (aborted, values, ops)              = Prelude.foldl constantFold' (False, M.empty, []) statements
        valueOps                            = if aborted
            then []
            else valuesToOps values
