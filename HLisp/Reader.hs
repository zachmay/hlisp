module HLisp.Reader (readProgram, readExpression) where

import HLisp.SExpr
import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as T
import Data.Char (toUpper)

{- readExpression - Takes a string
 - and returns a parsing result. This is the only exported function. -}
readExpression = runParser sExpr "" ""
readProgram = runParser program "" ""

{- Valid symbols for identifiers.
 - This is somewhat arbitrary and may not correspond to standard Lisp. -}
atomSymbols = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "_:!#$%&*+/<=>?@\\^|-~" 

{- Set up some of Parsec's nice language parsing features. -}
lispStyle = emptyDef { T.commentStart  = "|#"
                     , T.commentEnd    = "#|"
                     , T.commentLine   = ";"
                     , T.identStart    = oneOf atomSymbols
                     , T.identLetter   = oneOf atomSymbols
                     , T.caseSensitive = False }

lisp = T.makeTokenParser lispStyle

{- Basic token types -}
integer = do
    sign <- option '+' (oneOf "+-")
    number <- T.natural lisp
    if sign == '+' then
       return number
    else
       return $ negate number
float         = T.float lisp
identifier    = T.identifier lisp
symbol        = T.symbol lisp
stringLiteral = T.stringLiteral lisp

{- A program is:
 - * One or more S-expressions. -}
program = many1 sExpr

{- an S-Expression is:
 - * A list of S-Expressions; or,
 - * An atom -}
sExpr =  try list
     <|> try atom
     <|> tickQuote

{- A list of S-Expressions is:
 - * Between open- and close-parentheses,  
 - * One or more S-Expressions,
 - The result is consify applied to the list of S-Expressions -}
list = between (symbol "(") (symbol ")") $ do
    sexprs <- many1 sExpr
    return $ consify sexprs

{- An atom is:
 - * a floating-point literal; or,
 - * an integer literal; or,
 - * the special atom T; or,
 - * the special atom NIL; or,
 - * a symbol atom; or,
 - * a string atom. -}
atom  =  try floatAtom
     <|> try intAtom
     <|> nilAtom
     <|> symbolAtom
     <|> stringAtom

{- A tick-quoted S-expression is:
 - * A literal single-quote;
 - * followed by an S-expression. -}
tickQuote = do
    symbol "'"
    s <- sExpr
    return $ Cons (Atom "QUOTE") (Cons s Nil)

intAtom = do
  n <- integer
  return (IntAtom n)

floatAtom = do
  x <- float
  return (FloatAtom x)

nilAtom = do
  try (symbol "()") <|> try (symbol "nil")
  return Nil

symbolAtom = do
  s <- identifier
  if s == "T" || s == "t" then
      return TrueAtom
  else
      return (Atom $ map toUpper s)

stringAtom = do
  s <- stringLiteral 
  return (StringAtom s)

{- consify: Take a list of SExprs and return them as a
 - chain of Cons'ed SExprs. -}
consify :: [SExpr] -> SExpr
consify [] = Nil
consify (x:xs) = Cons x (consify xs)
