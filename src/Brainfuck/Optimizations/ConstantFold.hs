module Brainfuck.Optimizations.ConstantFold (constantFold) where

import Data.Word
import Data.Char
import Data.Maybe
import Data.Either
import Data.Traversable
import qualified Data.Map.Lazy as M
import qualified Data.Set as S

import Brainfuck

getChangedOffs _ []                 = []
getChangedOffs shift (curr:rest)
    | isLoop curr                   = (getChangedOffs shift children) ++ restOffs
    | isJust off                    = (shift + fromJust off) : restOffs
    | otherwise                     = restOffs
    where
        Loop _ children             = curr
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
        valueToOp off (Just val)    = Set off (Const $ fromIntegral val)
        valueOpMap                  = M.mapWithKey valueToOp $ M.filter isJust values
        valueOps                    = map snd $ M.toList valueOpMap

evaluateExpr defVal values expr     = case expr of
    Const _                         -> expr
    Var off mul                     -> case getValue off of
        Just val                    -> Const $ fromIntegral $ (fromIntegral mul) * (fromIntegral val)
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
                    Just val        -> Left $ (fromIntegral mul) * (fromIntegral val)
                    Nothing         -> Right (off, fromIntegral mul)
                varVals             = map (uncurry getVar) vars
                valSum              = (fromIntegral val) + (sum $ lefts varVals)
                nonConstVars        = rights varVals
                singleNonConstVar   = valSum == 0 && length nonConstVars == 1

constantFold' (defVal, values, ops) curr    = case curr of
    Add off expr                            -> case getValue off of
        Nothing                             -> (defVal, values, (Add off $ evaluateExpr defVal values expr) : ops)
        Just val2                           -> case evaluateExpr defVal values expr of
            Const val1                      -> (defVal, setValue off (Just $ (fromIntegral val1) + val2), ops)
            expr'                           -> (defVal, setValue off Nothing, simplifiedExpr : ops)
                where
                    simplifiedExpr          = Add off $ addExpressions expr' (Const $ fromIntegral val2)

    Set off expr                            -> case evaluateExpr defVal values expr of
        Const val                           -> (defVal, setValue off (Just $ fromIntegral val), ops)
        expr'                               -> (defVal, setValue off Nothing, (Set off expr') : ops)

    Shift off                               -> (defVal, M.mapKeys (subtract off) values, curr : ops)

    Loop condition children
        | conditionZero                     -> (defVal, values, ops)
        | canEval                           -> evalLoop (defVal, values, ops)
        | zeroShift                         -> (defVal, newValues', curr : usedValueOps ++ ops)
        | otherwise                         -> (Nothing, M.empty, curr : valueOps ++ ops)
        where
            conditionZero                   = (fromMaybe 1 $ getValue condition) == 0
            zeroShift                       = isZeroShift children
            offsetsKnown                    = offsetIsKnown condition && all offsetIsKnown burntOffsets
            canEval                         = zeroShift && offsetsKnown && (not $ any blocksLoopEval children)
            burntOffsets                    = getChangedOffs 0 children
            burntValues                     = M.restrictKeys values (S.fromList burntOffsets)
            usedValueOps                    = valuesToOps burntValues
            newValues                       = foldl (\m off -> M.insert off Nothing m) values burntOffsets
            newValues'                      = M.insert condition (Just 0) newValues
            offsetIsKnown off               = definedOldOffset || definedNewOffset
                where
                    hit                     = M.lookup off values
                    definedNewOffset        = isNothing hit && isJust defVal
                    definedOldOffset        = isJust hit && (isJust $ fromJust hit)
            blocksLoopEval stmt             = case stmt of
                Loop _ children             -> any blocksLoopEval children
                Shift _                     -> True
                Input _                     -> True
                _                           -> False
            --TODO detect infinite loops so we dont loop infinitely
            evalLoop state@(_, values, _)   = if (fromMaybe 0 $ fromMaybe defVal $ M.lookup condition values) == 0
                then state
                else evalLoop $ foldl constantFold' state children

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
        --the typing of 0 here is important as from its type the whole type
        --structure of constantFold' can be deduced to work on Word8's so that
        --0 - 1 == 255 (just as in brainfuck)
        (_, values, ops)                    = foldl constantFold' (Just (0 :: Word8), M.empty, []) statements
        valueOps                            = valuesToOps values
