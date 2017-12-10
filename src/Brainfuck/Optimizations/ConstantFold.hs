module Brainfuck.Optimizations.ConstantFold where

import Data.Char
import Data.Maybe
import Data.Either
import Data.Traversable
import qualified Data.Map.Lazy as M

import Brainfuck

getChangedOffs _ []                 = []
getChangedOffs shift (curr:rest)
    | isLoop curr                   = (getChangedOffs shift children) ++ restOffs
    | isJust off                    = (shift + fromJust off) : restOffs
    | otherwise                     = restOffs
    where
        Loop children               = curr
        newShift                    = case curr of
            Shift x                 -> shift + x
            _                       -> shift
        off                         = case curr of
            Set off _               -> Just off
            Add off _               -> Just off
            Input off               -> Just off
            _                       -> Nothing
        restOffs                    = getChangedOffs newShift rest

valuesToOps values                  = valueOps
    where
        valueToOp off (Just val)    = Set off (Const val)
        valueOpMap                  = M.mapWithKey valueToOp $ M.filter isJust values
        valueOps                    = map snd $ M.toList valueOpMap

evaluateExpr defVal values expr     = case expr of
    Const _                         -> expr
    Var off mul                     -> case getValue off of
        Just val                    -> Const $ val * mul
        Nothing                     -> expr
    Sum val vars                    -> evaluateSum val vars
    where
        getValue off                = fromMaybe defVal $ M.lookup off values
        evaluateSum val vars
            | null nonConstVars     = Const valSum
            | singleNonConstVar     = Var (fst $ head nonConstVars) (snd $ head nonConstVars)
            | otherwise             = Sum valSum nonConstVars
            where
                getVar off mul      = case getValue off of
                    Just val        -> Left $ val * mul
                    Nothing         -> Right (off, mul)
                varVals             = map (uncurry getVar) vars
                valSum              = val + (sum $ lefts varVals)
                nonConstVars        = rights varVals
                singleNonConstVar   = valSum == 0 && length nonConstVars == 1

--TODO do we really need to reset values when we encounter a Loop?
constantFold' (defVal, values, ops) curr    = case curr of
    Add off expr                            -> case getValue off of
        Nothing                             -> (defVal, values, (Add off $ evaluateExpr defVal values expr) : ops)
        Just val2                           -> case evaluateExpr defVal values expr of
            Const val1                      -> (defVal, setValue off (Just $ val1 + val2), ops)
            expr'                           -> (defVal, setValue off Nothing, simplifiedExpr : ops)
                where
                    simplifiedExpr          = Add off $ addExpressions expr' (Const val2)

    Set off expr                            -> case evaluateExpr defVal values expr of
        Const val                           -> (defVal, setValue off (Just val), ops)
        expr'                               -> (defVal, setValue off Nothing, (Set off expr') : ops)

    Shift _                                 -> (Nothing, M.empty, curr : valueOps ++ ops)

    Loop children                           -> if isZeroShift children
        then (defVal, newValues, curr : ops)
        else (Nothing, M.empty, curr : valueOps ++ ops)
        where
            burntOffsets                    = getChangedOffs 0 children
            newValues                       = foldl (\m off -> M.insert off Nothing m) values burntOffsets

    Input off                               -> (defVal, setValue off Nothing, curr : ops)

    Output expr                             -> case evaluateExpr defVal values expr of
        Const val                           -> case firstOp of
            Just (Print str)                -> (defVal, values, (Print $ str ++ [chr val]) : tail ops)
            _                               -> (defVal, values, (Print [chr val]) : ops)
        expr'                               -> (defVal, values, (Output expr') : ops)

    Comment _                               -> (defVal, values, curr : ops)

    where
        setValue off val                    = M.insert off val values
        getValue off                        = fromMaybe defVal $ M.lookup off values
        valueOps                            = valuesToOps values
        firstOp                             = if null ops
            then Nothing
            else Just $ head ops

constantFold statements                     = reverse $ valueOps ++ ops
    where
        (_, values, ops)                    = foldl constantFold' (Just 0, M.empty, []) statements
        valueOps                            = valuesToOps values
