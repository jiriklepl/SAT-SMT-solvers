
\documentclass{article}
%include polycode.fmt
\begin{document}

First, we want some imports:

\section{Parser}

\begin{code}
module Main where
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text
import Data.Void
import Control.Monad.State as State
import Control.Monad (void)
\end{code}

Then, we add some boilerplate:

\begin{code}

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

packSymbol :: String -> Parser Text
packSymbol = symbol . pack

parens :: Parser a -> Parser a
parens = between (packSymbol "(") (packSymbol ")")
\end{code}

The input file contains a description of a single formula in NNF using a very simplified SMT-LIB format following grammatical rules:

\begin{verbatim}
<formula> ::= `(' `and' <formula> <formula> `)'
          | `(' `or' <formula> <formula> `)'
          | `(' `not' <variable> `)' 
          | <variable>
\end{verbatim}

We rewrite those into haskell:

\begin{code}
formula :: Parser Formula
formula = parens $ do packSymbol "and" >> FAnd <$> formula <*> formula
                   <|> do packSymbol "or" >> FOr <$> formula <*> formula
                   <|> do packSymbol "not" >> FNot <$> variable

variable :: Parser Variable
variable = lexeme $ (pure <$> letterChar) <> many alphaNumChar 
\end{code}

And then we define the supporting types:

\begin{code}
type Parser = Parsec Void Text

data Formula = FAnd Formula Formula
             | FOr Formula Formula
             | FNot Variable
             | FVar Variable
             deriving (Eq, Ord)

type Variable = String
\end{code}

\section{Encoding}

Now we can define the encoding:

\begin{code}

encode' :: Formula -> Encoder ()
encode' (FAnd left right) = 0 

\end{code}



\end{document}