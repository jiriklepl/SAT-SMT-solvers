{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as Set
import Data.List
import Data.Foldable
import Data.Maybe
import Data.Time
import Control.Monad.State
import Control.Monad

import Tseitin

type Assignment = [Int]

data SatisfierState = SatState
    { satDecs :: [Int]
    , satImpls :: [Int]
    }

type Satisfier = StateT SatisfierState []

instance Semigroup (Satisfier a) where
    (StateT f) <> (StateT g) = StateT (\s -> do
        (a', s') <- f s
        (a', s') : g s')

instance Monoid (Satisfier a) where
    mempty = StateT $ const []

satisfy :: Cnf -> [(Assignment, Cnf, [Int], [Int])]
satisfy cnf = do
    ((assigns', cnf'), state') <- runStateT (satisfy' [] cnf) SatState{satDecs=[],satImpls=[]}
    return (assigns', cnf', satDecs state', satImpls state')

satisfy' :: Assignment -> Cnf -> Satisfier (Assignment, Cnf)
satisfy' assigns cnf = do 
    state@(assigns', cnf') <- satisfyStep assigns cnf
    if listToMaybe assigns' /= listToMaybe assigns
        then satisfy' assigns' cnf'
        else return state

satisfyStep :: Assignment -> Cnf -> Satisfier (Assignment, Cnf)
satisfyStep assigns cnf
    | cnf == mempty = return (assigns, cnf)
    | Clause mempty `Set.member` cnf = mempty
    | otherwise = do
        let Clause minClause = Set.findMin cnf
            l = Set.findMin minClause
            cnf' = assign l cnf
            cnf'' = assign (negate l) cnf
        if Set.size minClause > 1 
            then do
                do
                    modify (\s -> s{satDecs = l : satDecs s})
                    return (l:assigns, cnf')
                <> do
                    modify (\s -> s{satDecs = -l : satDecs s})
                    return (-l:assigns, cnf'')
            else do
                modify (\s -> s{satImpls = l : satImpls s})
                return (l:assigns, cnf') -- Unit propagation here

assign :: Int -> Cnf -> Cnf
assign l cnf =
    let cnf' = Set.filter (\(Clause c) -> not $ l `Set.member` c) cnf
    in Set.map (\(Clause c) -> Clause $ negate l `Set.delete` c) cnf'

main :: IO ()
main = do
    args <- getArgs
    let (options, arguments) = partition
            (\case '-':'-':rest -> True
                   _ -> False)
            args
        implOpt = "--impl" `elem` options
        (input, output) = case arguments of
            [i, o] -> (Just i, Just o)
            [i] -> (Just i, Nothing)
            [] -> (Nothing, Nothing)
            _ -> error "Invalid format"
    inputHandle <- traverse (`openFile` ReadMode) input
    contents <- maybe T.getContents T.hGetContents inputHandle
    let result = do
            f <- runParse (fromMaybe "stdin" input) contents
            return $ runEncode f implOpt
    traverse_ hClose inputHandle
    case result of
        Right result -> do
            case output of
                Nothing -> prettyPrint stdout result
                Just o -> do
                    handle <- openFile o WriteMode
                    prettyPrint handle result
                    hClose handle
        Left e -> print e

prettyPrint :: Handle -> EncoderState -> IO ()
prettyPrint handle state = do
    start <- getCurrentTime
    let (bad, right) = span (\(assigns, cnf, decs, impls) -> Clause mempty `Set.member` cnf) $ satisfy (cnfRepr state)
        (sumDecs, sumImpls)  = foldr (\(_, _, decs, impls) (s, i) -> (s + length decs, i + length impls)) (0, 0) bad
    case right of
        [] -> hPrint handle "Cannot satisfy"
        all@((assigns, cnf, decs, impls):_) -> do
            hPutStrLn handle $ "Satisfied by " <> show assigns
            hPutStrLn handle $ "Decisions: " <> show decs
            hPutStrLn handle $ "Implied variables: " <> show impls
            hPutStrLn handle $
                "Found after " <> show (sumDecs + length decs) <>
                " decisions and " <> show (sumImpls + length impls) <>
                " implied assignments (wrong attempts: " <> show (length bad) <> ")"
    end <- getCurrentTime
    hPutStrLn handle $ "DPLL took " <> show (diffUTCTime end start)