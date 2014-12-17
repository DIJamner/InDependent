module TypedLambdaParser where
import System.Environment
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Errors as E

import TypedLambda

varIdentifier :: Parser String
varIdentifier = (many1 $ alphaNum) <|> (many1 $ oneOf "+-*/&|=<>")

lambda :: [String] -> Parser LambdaExp
lambda prims = do
        char '\\'
        x <- varIdentifier
        spaces
        char ':'
        spaces
        t <- typeExp prims
        char '.'
        body <- lambdaExp prims
        return $ Lambda x t body
        
apply :: [String] -> Parser LambdaExp
apply prims = do
        char '('
        t1 <- lambdaExp prims
        skipMany1 space
        t2 <- lambdaExp prims
        char ')'
        return $ Apply t1 t2
        
variable :: Parser LambdaExp
variable = do
        x <- varIdentifier
        return $ Variable x


primitiveType :: [String] -> Parser SimpleType
primitiveType prims = do
                x <- choice $ map string prims
                return $ Primitive x

functionType :: [String] -> Parser SimpleType
functionType prims = do
        x <- typeExp prims
        spaces
        string "->"
        spaces
        y <- typeExp prims
        return $ Function x y


parens :: Parser a -> Parser a
parens p = do
        char '('
        spaces
        e <- p
        spaces
        char ')'
        return e

typeExp :: [String] -> Parser SimpleType
typeExp prims = do
                t <- spaces >> (primitiveType prims <|> (parens $ typeExp prims))
                ts <- many $ (string "->") >> (typeExp prims)
                return $ typeListToFunc (t:ts)

typeListToFunc :: [SimpleType] -> SimpleType
typeListToFunc (t:[]) = t
typeListToFunc (t:ts) = Function t (typeListToFunc ts)

lambdaExp :: [String] -> Parser LambdaExp
lambdaExp prims = (lambda prims) <|> (apply prims) <|> variable


readExpr :: String -> [String] -> Either ParseError LambdaExp
readExpr input prims = parse (lambdaExp prims) "typed lambda" input