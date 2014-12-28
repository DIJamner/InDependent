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
        (r, t) <- lArgType
        char '.'
        e <- expr
        return $ abstract (Lambda r) x t e
        
lArgType :: Parser (Req, Expr)
lArgType = (do
        char '{'
        spaces
        t <- expr
        spaces
        char '}'
        return (Impl, t))
        <|>(do 
                t <- expr
                return (Expl, t))

piType :: Parser Expr
piType = do
        (arg, r, at) <- pArgType
        spaces
        string "->"
        spaces
        resType <- expr
        --this optimization short circuits the abstraction for non-dependent terms
        return $ if arg == "!NONE" then Pi r at resType 
                else abstract (Pi r) arg at resType

pArgType :: Parser (String, Req, Expr)
pArgType = (try dependentType) <|> do
                        (r, at) <- (between (char '{') (char '}') $ do
                                spaces
                                t <- typeableExpr
                                spaces
                                return (Impl, t))
                                <|> do
                                        t <- typeableExpr
                                        return (Expl, t)
                                <?> "non dependent type"
                        return ("!NONE", r, at)
                <?> "function type"

dependentType :: Parser (String, Req, Expr)
dependentType = (do
        char '('
        spaces
        a <- var
        spaces
        char ':'
        spaces
        let r = Expl
        at <- typeableExpr
        spaces
        char ')'
        return (a, r, at))
        <|> (do
                char '{'
                spaces
                a <- var
                spaces
                char ':'
                spaces
                let r = Impl
                at <- typeableExpr
                spaces
                char '}'
                return (a, r, at))
                


apply :: Parser Expr
apply = do
        char '('
        a <- expr
        args <- many1 (do spaces;expr)
        char ')'
        return $ foldl (\a b -> Apply a b) a args

