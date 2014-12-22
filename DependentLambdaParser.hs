module DependentLambdaParser where
import System.Environment
import Control.Monad

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Errors as E
import DependentLambda

expr :: Parser Expr
expr = universe 
        <|> lambda
        <|> try piType
        <|> try apply
        <|> Var `fmap` (Ref `fmap` var)
        <|> do
                char '('
                e <- expr
                char ')'
                return e
        <?> "expression"

var :: Parser String
var = (do 
                a <- letter
                b <- many1 $ alphaNum
                return $ a:b)
        <|> (many1 $ oneOf "+-*/&|=<>")

universe :: Parser Expr
universe = (string "Type") >> (fmap Universe $ lInt)

lInt :: Parser Int
lInt = rd `fmap` (many1 digit)
        where rd = read :: String -> Int

lambda :: Parser Expr
lambda = do
        char '\\'
        x <- var
        spaces
        char ':'
        spaces
        t <- expr
        char '.'
        e <- expr
        return $ abstract Lambda x t e
        

piType :: Parser Expr
piType = do
        (arg, argType) <- (try $ do
                        char '('
                        spaces
                        a <- var
                        spaces
                        char ':'
                        spaces
                        argType <- expr
                        spaces
                        char ')'
                        return (a, argType)
                ) <|> do
                        argType <- expr
                        return ("!NONE", argType)           
        spaces
        string "->"
        spaces
        resType <- expr
        --this optimization short circuits the abstraction for non-dependent terms
        return $ if arg == "!NONE" then Pi argType resType 
                else abstract Pi arg argType resType

apply :: Parser Expr
apply = do
        a <- expr
        skipMany1 space
        b <- expr
        return $ Apply a b

