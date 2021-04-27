{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Foldable
import Data.Maybe
import Data.Time
import Control.Monad.State

import Tseitin
import DIMACS
import Clause

type Assignment = [Int]

data InputFormat = FormatSat | FormatCnf

data SatisfierState = SatState
    { satDecs :: [Int]
    , satImpls :: [Int]
    }

type Satisfier = StateT SatisfierState []

instance Semigroup (Satisfier a) where
    (StateT f) <> (StateT g) = StateT (\s -> do
        (a, s') <- f s
        (a, s') : g s')

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
        dimacsOpt = "--cnf" `elem` options
        satlibOpt = "--sat" `elem` options
        (input, output) = case arguments of
            [i, o] -> (Just i, Just o)
            [i] -> (Just i, Nothing)
            [] -> (Nothing, Nothing)
            _ -> error "Invalid format"
    inputHandle <- traverse (`openFile` ReadMode) input
    contents <- maybe T.getContents T.hGetContents inputHandle
    let inputFormat = case (dimacsOpt, satlibOpt, input) of
            (False, False, Just i) -> do
                let suffix = reverse . takeWhile (/= '.') $ reverse i
                case suffix of
                    "sat" -> FormatSat
                    "cnf" -> FormatCnf
                    _ -> error "Unrecognized format"
            (False, False, Nothing) -> error "Cannot derive the requested format"
            (True, True, _) -> error "Requested format ambiguous"
            (True, _, _) -> FormatCnf
            (_, True, _) -> FormatSat
    let result = case inputFormat of
            FormatSat -> do
                f <- Tseitin.runParse (fromMaybe "stdin" input) contents
                let state = runEncode f False
                    names = Map.fromAscList
                        [ (value, name)
                        | (FVar name, value) <- Map.toList (formulaNames state)
                        ]
                return (cnfRepr state, names)
            FormatCnf -> do
                cnf <- DIMACS.runParse (fromMaybe "stdin" input) contents
                return (cnf, mempty)
    traverse_ hClose inputHandle
    case result of
        Right (cnf, names) -> do
            case output of
                Nothing -> runSatisfier stdout cnf names
                Just o -> do
                    handle <- openFile o WriteMode
                    runSatisfier handle cnf names
                    hClose handle
        Left e -> print e

runSatisfier :: Handle -> Cnf -> Map.Map Int String -> IO ()
runSatisfier handle cnf names = do
    start <- getCurrentTime
    let (bad, right) = span (\(assigns, cnf', decs, impls) -> Clause mempty `Set.member` cnf') $ satisfy cnf
        (sumDecs, sumImpls)  = foldr (\(_, _, decs, impls) (s, i) -> (s + length decs, i + length impls)) (0, 0) bad
    end <- case right of
        [] -> do
            end <- getCurrentTime
            hPrint handle "Cannot satisfy"
            hPutStrLn handle $
                "Found after " <> show sumDecs <>
                " decisions and " <> show sumImpls <>
                " implied assignments (wrong attempts: " <> show (length bad) <> ")"
            return end
        (assigns, _, decs, impls):_ -> do
            end <- getCurrentTime
            hPutStr handle "Satisfied by "
            if names == mempty
                then hPrint handle (sortBy (\a b -> abs a `compare` abs b) assigns)
                else hPrint handle $ catMaybes
                    ((\i -> if i > 0 then i `Map.lookup` names else ('-' :) <$> negate i `Map.lookup` names) <$> assigns)
            hPutStrLn handle $ "Decisions: " <> show decs
            hPutStrLn handle $ "Implied assignments: " <> show impls
            hPutStrLn handle $
                "Found after " <> show (sumDecs + length decs) <>
                " decisions and " <> show (sumImpls + length impls) <>
                " implied assignments (wrong attempts: " <> show (length bad) <> ")"
            sequence_
                [ hPutStrLn handle (value ++ " = " ++ show key)
                | (key, value) <- Map.toList names
                ]
            return end
    hPutStrLn handle $ "DPLL took " <> show (diffUTCTime end start)
    