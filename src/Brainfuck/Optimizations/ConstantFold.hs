module Brainfuck.Optimizations.ConstantFold where

import Data.Maybe
import Data.Traversable
import Data.Map.Lazy as M

import Brainfuck

valuesToOps values                  = valueOps
    where
        valueToOp off (Just val)    = Set off val
        valueOpMap                  = M.mapWithKey valueToOp $ M.filter isJust values
        valueOps                    = Prelude.map snd $ toList valueOpMap

constantFold' (True, values, ops) curr                  = (True, values, curr : ops)
constantFold' (False, values, ops) curr                 = case curr of
    Math off val1                           -> case getValue off of
        Nothing                             -> (False, values, curr : ops)
        Just val2                           -> (False, setValue off (Just $ val1 + val2), ops)
    Set off val                             -> (False, setValue off (Just val), ops)
    Copy off1 off2 mul add                  -> case getValue off2 of
        Nothing                             -> (False, setValue off1 Nothing, ops)
        Just val                            -> (False, setValue off1 (Just $ val * mul + add), ops)
    Shift _                                 -> (True, values, curr : valueOps ++ ops)
    Loop _                                  -> (True, values, curr : valueOps ++ ops)
    Input off                               -> (False, setValue off Nothing, ops)
    Output off                              -> case getValue off of
        Nothing                             -> (False, values, curr : ops)
        Just val                            -> (False, values, curr : (Set off val) : ops)
    Comment _                               -> (False, values, curr : ops)
    where
        setValue off val                    = M.insert off val values
        getValue off                        = fromMaybe (Just 0) $ M.lookup off values
        valueOps                            = valuesToOps values

constantFold statements                     = reverse $ valueOps ++ ops
    where
        (aborted, values, ops)              = Prelude.foldl constantFold' (False, M.empty, []) statements
        valueOps                            = if aborted
            then []
            else valuesToOps values
