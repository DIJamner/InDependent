module DependentLambdaParser where
import Control.Monad

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Errors as E
import DependentLambda

expr :: Parser Expr
expr = lambda
        <|> try piType
        <|> try universe
        <|> try apply
        <|> (Var `fmap` (Ref `fmap` var))
        <|> do
                char '('
                e <- expr
                char ')'
                return e
        <?> "expression"

--parses expressions that can appear in types
typeableExpr :: Parser Expr
typeableExpr = try universe
        <|> try apply
        <|> (Var `fmap` (Ref `fmap` var))
        <|> do
                char '('
                e <- typeableExpr
                char ')'
                return e
        <?> "typeable expression"

var :: Parser String
var = (do 
                a <- letter
                b <- many $ alphaNum
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
piType = do --TODO: causes to run in infinite loop
        -- (arg, argType) <- (try $ do
 --                        char '('
 --                        spaces
 --                        a <- var
 --                        spaces
 --                        char ':'
 --                        spaces
 --                        argType <- expr
 --                        spaces
 --                        char ')'
 --                        return (a, argType)
 --                ) <|> do
 --                        argType <- expr
 --                        return ("!NONE", argType)
 --                <?> "dependent product type"      
 
        --TEMP
        let arg = "!NONE"
        argType <- typeableExpr
        --END_TEMP
        spaces
        string "->"
        spaces
        resType <- expr
        --this optimization short circuits the abstraction for non-dependent terms
        return $ if arg == "!NONE" then Pi argType resType 
                else abstract Pi arg argType resType

apply :: Parser Expr
apply = do
        char '('
        a <- expr
        skipMany1 space
        b <- expr
        char ')'
        return $ Apply a b

