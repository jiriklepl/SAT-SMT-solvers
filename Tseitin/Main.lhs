
\documentclass{article}
%include polycode.fmt
\begin{document}

First, we want some imports:

\section{Parser}

\begin{code}
{-# LANGUAGE LambdaCase #-}
module Main where
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Data.Foldable
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Control.Monad.State 
import System.Environment
import System.IO
import Control.Monad (void)
\end{code}

Then, we add some boilerplate:

\begin{code}

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

packSymbol :: String -> Parser T.Text
packSymbol = symbol . T.pack

parens :: Parser a -> Parser a
parens = between (packSymbol "(") (packSymbol ")")
\end{code}

We rewrite the following grammar into haskell:

\begin{verbatim}
<formula> ::= `(' `and' <formula> <formula> `)'
          | `(' `or' <formula> <formula> `)'
          | `(' `not' <variable> `)' 
          | <variable>
\end{verbatim}

\begin{code}
formula :: Parser Formula
formula =  try ( parens $ do packSymbol "and" >> FAnd <$> formula <*> formula
                      <|> do packSymbol "or" >> FOr <$> formula <*> formula
                      <|> do packSymbol "not" >> FNeg . FVar <$> variable
               )
            <|> do FVar <$> variable

variable :: Parser Variable
variable = lexeme $ (pure <$> letterChar) <> many alphaNumChar 
\end{code}

And then we define the supporting types:

\begin{code}
type Parser = Parsec Void T.Text

data Formula = FAnd Formula Formula
             | FOr Formula Formula
             | FNeg Formula
             | FVar Variable
             deriving (Eq, Ord, Show)

type Variable = String
\end{code}

\section{Encoding}

Now we can define the encoding:

\begin{code}
encode :: Formula -> Encoder ()
encode f = do
    name <- encode' f
    modify (\state -> state{cnfRepr=Set.singleton name `Set.insert` cnfRepr state})

encode' :: Formula -> Encoder Int
encode' f@(FAnd left right) = do
    leftName <- encode' left
    rightName <- encode' right
    name <- getName f
    modify (\state ->
        state{cnfRepr=Set.fromList [-name, leftName] `Set.insert` cnfRepr state})
    modify (\state ->
        state{cnfRepr=Set.fromList [-name, rightName] `Set.insert` cnfRepr state})
    gets equiv >>= flip when (modify (\state ->
        state{cnfRepr=Set.fromList [-leftName, -rightName, name] `Set.insert` cnfRepr state}))
    return name

encode' (FOr left right) = encode' . FNeg $ FAnd (FNeg left) (FNeg right)
encode' (FNeg f) = negate <$> encode' f
encode' f@FVar{} = getName f

getName :: Formula -> Encoder Int
getName f = do
    state@EncoderState{formulaNames=names} <- get
    case f `Map.lookup` names of
        Just i -> return i
        Nothing -> do
            let name = Map.size names + 1
            put state{formulaNames=Map.insert f name names}
            return name


\end{code}

\begin{code}
type Encoder = State EncoderState

data EncoderState = EncoderState
    { formulaNames :: Map.Map Formula Int
    , cnfRepr :: Set.Set (Set.Set Int)
    , equiv :: Bool
    }
\end{code}


\begin{code}

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
        | clause <- toList cnf
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
            f <- parse formula (fromMaybe "stdin" input) contents
            return $ execState
                (encode f)
                EncoderState{formulaNames=mempty, cnfRepr=mempty, equiv=not implOpt}
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
\end{code}


\end{document}