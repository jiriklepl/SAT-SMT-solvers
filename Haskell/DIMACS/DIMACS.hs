module DIMACS where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import Data.Void
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

import Clause

{-
    First, we define some boilerplate code
-}

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

symbol :: T.Text -> Parser T.Text
symbol = L.symbol hspace

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

comment :: Parser ()
comment = L.skipLineComment (T.pack "c") >> space

header :: Parser ()
header = L.skipLineComment (T.pack "p") >> space

clause :: Parser [Int]
clause = do
    lit <- literal
    others <- many literal
    let lits = reverse (lit:others)
    if head lits == 0
        then return . reverse . tail $ lits
        else fail "Every clause has to end with 0"

literal :: Parser Int
literal = lexeme (L.decimal :: Parser Int)
        <|> negate <$> (packSymbol "-" >> lexeme (L.decimal :: Parser Int))


dimacs :: Parser Cnf
dimacs = do
    void $ many comment
    void header
    Set.fromList . (Clause . Set.fromList <$>) <$> many (do
        c <- clause
        space
        return c)

runParse = parse dimacs
