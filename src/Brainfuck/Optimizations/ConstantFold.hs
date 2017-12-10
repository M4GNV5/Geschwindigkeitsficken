module Brainfuck.Optimizations.ConstantFold where

import Data.Char
import Data.Maybe
import Data.Either
import Data.Traversable
import qualified Data.Map.Lazy as M

import Brainfuck

valuesToOps values                  = valueOps
    where
        valueToOp off (Just val)    = Set off (Const val)
        valueOpMap                  = M.mapWithKey valueToOp $ M.filter isJust values
        valueOps                    = map snd $ M.toList valueOpMap

evaluateExpr values expr            = case expr of
    Const _                         -> expr
    Var off mul                     -> case getValue off of
        Just val                    -> Const $ val * mul
        Nothing                     -> expr
    Sum val vars                    -> evaluateSum val vars
    where
        getValue off                = fromMaybe (Just 0) $ M.lookup off values
        evaluateSum val vars
            | null nonConstVars     = Const valSum
            | singleNonConstVar     = Var (fst $ head nonConstVars) (snd $ head nonConstVars)
            | otherwise             = Sum valSum nonConstVars
            where
                getVar :: Int -> Int -> Either Int (Int, Int)
                getVar off mul      = case getValue off of
                    Just val        -> Left $ val * mul
                    Nothing         -> Right (off, mul)
                varVals             = map (uncurry getVar) vars
                valSum              = val + (sum $ lefts varVals)
                nonConstVars        = rights varVals
                singleNonConstVar   = valSum == 0 && length nonConstVars == 1

--TODO do we really need to reset values when we encounter a Loop?
constantFold' (values, ops) curr            = case curr of
    Add off expr                            -> case getValue off of
        Nothing                             -> (values, (Add off $ evaluateExpr values expr) : ops)
        Just val2                           -> case evaluateExpr values expr of
            Const val1                      -> (setValue off (Just $ val1 + val2), ops)
            expr'                           -> let
                simplifiedExpr              = Add off $ addExpressions expr' (Const val2)
                in (setValue off Nothing, simplifiedExpr : ops)
    Set off expr                            -> case evaluateExpr values expr of
        Const val                           -> (setValue off (Just val), ops)
        expr'                               -> (setValue off Nothing, (Set off expr') : ops)
    Shift _                                 -> (M.empty, curr : valueOps ++ ops)
    Loop _                                  -> (M.empty, curr : valueOps ++ ops)
    Input off                               -> (setValue off Nothing, curr : ops)
    Output expr                             -> case evaluateExpr values expr of
        Const val                           -> case firstOp of
            Just (Print str)                -> (values, (Print $ str ++ [chr val]) : tail ops)
            _                               -> (values, (Print [chr val]) : ops)
        expr'                               -> (values, (Output expr') : ops)
    Comment _                               -> (values, curr : ops)
    where
        setValue off val                    = M.insert off val values
        getValue off                        = fromMaybe (Just 0) $ M.lookup off values
        valueOps                            = valuesToOps values
        firstOp                             = if null ops
            then Nothing
            else Just $ head ops

constantFold statements                     = reverse $ valueOps ++ ops
    where
        (values, ops)                       = foldl constantFold' (M.empty, []) statements
        valueOps                            = valuesToOps values
