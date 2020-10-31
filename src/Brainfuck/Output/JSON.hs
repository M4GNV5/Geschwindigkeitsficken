module Brainfuck.Output.JSON (dumpStatements) where

import Data.List
import Data.Ratio
import Control.Monad.State

import qualified Data.Map.Lazy as M
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BS

import Brainfuck

toJSON :: JSON.ToJSON a => a -> JSON.Value
toJSON = JSON.toJSON

jsonObj :: [(String, JSON.Value)] -> JSON.Value
jsonObj                         = JSON.toJSON . M.fromList

jsonifyExpression :: Expression -> JSON.Value
jsonifyExpression expr          = case expr of
    Const x                     -> jsonifyExpression $ Sum x []
    Var off mul                 -> jsonifyExpression $ Sum 0 [(off, mul)]
    Sum val vars                -> jsonObj [("scalar", toJSON val), ("summands", varsJson)]
        where
            varToJson (off,mul) = jsonObj [("offset", toJSON off), ("scale", toJSON $ mul)]
            varsJson            = toJSON $ map varToJson vars

jsonifyStatement :: Statement -> JSON.Value
jsonifyStatement stmt           = case stmt of
    Add off val                 -> jsonObj [("type", toJSON "add"), ("offset", toJSON off), ("value", jsonifyExpression val)]
    Set off val                 -> jsonObj [("type", toJSON "set"), ("offset", toJSON off), ("value", jsonifyExpression val)]
    Shift off                   -> jsonObj [("type", toJSON "shift"), ("offset", toJSON off)]
    AddUntilZero off val        -> jsonObj [("type", toJSON "adduntilzero"), ("offset", toJSON off), ("value", toJSON val)]
    Loop off body               -> jsonObj [("type", toJSON "loop"), ("offset", toJSON off), ("body", toJSON $ map jsonifyStatement body)]
    Input off                   -> jsonObj [("type", toJSON "input"), ("offset", toJSON off)]
    Output val                  -> jsonObj [("type", toJSON "output"), ("value", jsonifyExpression val)]
    Print val                   -> jsonObj [("type", toJSON "print"), ("value", toJSON val)]
    Comment val                 -> jsonObj [("type", toJSON "comment"), ("value", toJSON val)]

dumpStatements statements       = BS.unpack $ JSON.encode $ toJSON $ map jsonifyStatement statements
