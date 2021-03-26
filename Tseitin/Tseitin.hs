module Tseitin where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State 

{-
    First, we define some boilerplate code
-}

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

packSymbol :: String -> Parser T.Text
packSymbol = symbol . T.pack

parens :: Parser a -> Parser a
parens = between (packSymbol "(") (packSymbol ")")

{-

We rewrite the following grammar into haskell:

<formula> ::= `(' `and' <formula> <formula> `)'
          | `(' `or' <formula> <formula> `)'
          | `(' `not' <variable> `)' 
          | <variable>

-}

data Formula = FAnd Formula Formula
             | FOr Formula Formula
             | FNeg Formula
             | FVar Variable
             deriving (Eq, Ord, Show)

type Variable = String

formula :: Parser Formula
formula =  try ( parens $ do packSymbol "and" >> FAnd <$> formula <*> formula
                      <|> do packSymbol "or" >> FOr <$> formula <*> formula
                      <|> do packSymbol "not" >> FNeg . FVar <$> variable
               )
            <|> do FVar <$> variable

variable :: Parser Variable
variable = lexeme $ (pure <$> letterChar) <> many alphaNumChar 

runParse = parse formula 

{-
    Then, we define the encoding
-}

type Encoder = State EncoderState

data EncoderState = EncoderState
    { formulaNames :: Map.Map Formula Int
    , cnfRepr :: Set.Set (Set.Set Int)
    , equiv :: Bool
    }

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

-- this generates a fresh name
getName :: Formula -> Encoder Int
getName f = do
    state@EncoderState{formulaNames=names} <- get
    case f `Map.lookup` names of
        Just i -> return i
        Nothing -> do
            let name = Map.size names + 1
            put state{formulaNames=Map.insert f name names}
            return name

runEncode f implOpt = execState
    (encode f)
    EncoderState{formulaNames=mempty, cnfRepr=mempty, equiv=not implOpt}