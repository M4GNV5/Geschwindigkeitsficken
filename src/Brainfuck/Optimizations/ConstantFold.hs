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

--TODO do we really need to reset values when we encounter a Loop?
constantFold' (values, ops) curr            = case curr of
    Math off val1                           -> case getValue off of
        Nothing                             -> (values, curr : ops)
        Just val2                           -> (setValue off (Just $ val1 + val2), ops)
    Set off _ 0 val                         -> (setValue off (Just val), ops)
    Set off1 off2 mul add                   -> case getValue off2 of
        Nothing                             -> (setValue off1 Nothing, ops)
        Just val                            -> (setValue off1 (Just $ val * mul + add), ops)
    Shift _                                 -> (M.empty, curr : valueOps ++ ops)
    Loop _                                  -> (M.empty, curr : valueOps ++ ops)
    Input off                               -> (setValue off Nothing, curr : ops)
    Output off                              -> case getValue off of
        Nothing                             -> (values, curr : ops)
        Just val                            -> case firstOp of
            Just (Print str)                -> (values, (Print $ str ++ [chr val]) : tail ops)
            _                               -> (values, (Print [chr val]) : ops)
    Comment _                               -> (values, curr : ops)
    where
        setValue off val                    = M.insert off val values
        getValue off                        = fromMaybe (Just 0) $ M.lookup off values
        valueOps                            = valuesToOps values
        firstOp                             = if Prelude.null ops
            then Nothing
            else Just $ head ops

constantFold statements                     = reverse $ valueOps ++ ops
    where
        (values, ops)                       = Prelude.foldl constantFold' (M.empty, []) statements
        valueOps                            = valuesToOps values
