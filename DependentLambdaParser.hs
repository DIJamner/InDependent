module DependentLambdaParser where
import Control.Monad

import Text.Parsec
import Text.Parsec.String (Parser)

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
var = do 
        a <- letter
        b <- many $ (alphaNum <|> char '|')
        return $ a:(replace '|' '.' b)
        
--utility function
replace :: Eq a => a -> a -> [a] -> [a]
replace a b [] = []
replace a b (c:cs) = if c == a then b:(replace a b cs) else c:(replace a b cs)

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
piType = do --TODO: clean up
        (arg, argType) <- (try $ do
                        char '('
                        spaces
                        a <- var
                        spaces
                        char ':'
                        spaces
                        argType <- typeableExpr
                        spaces
                        char ')'
                        return (a, argType)
                ) <|> do
                        argType <- typeableExpr
                        return ("!NONE", argType)
                <?> "function type"    
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
        args <- many1 (do spaces;expr)
        char ')'
        return $ foldl (\a b -> Apply a b) a args

