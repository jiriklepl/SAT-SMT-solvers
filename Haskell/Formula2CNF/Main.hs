{-# LANGUAGE LambdaCase #-}

module Main where
import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Foldable
import Data.Maybe

import Tseitin
import Clause

prettyPrint :: Handle -> EncoderState -> IO ()
prettyPrint handle EncoderState{formulaNames=names, cnfRepr=cnf} = do
    originalCount <- sequence
        [ case key of 
            FVar name -> hPutStrLn handle ("c " ++ name ++ " = " ++ show value) >> return 1
            _ -> return 0
        | (key, value) <- Map.toList names
        ]
    let totalCount = Map.size names
    hPutStrLn handle $ "c $root = " ++ show totalCount
    hPutStrLn handle $ "p cnf " ++ show totalCount ++ ' ' : show (length cnf)
    sequence_
        [ sequence_ [ hPutStr handle (show item ++ " ") | item <- Set.toList clause] >> hPutStrLn handle "0"
        | Clause clause <- toList cnf
        ]

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
