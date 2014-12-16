module UntypedLambda where
import Text.Parsec
import Text.Parsec.String (Parser)
import System.Environment


data LambdaExp
        = Lambda String LambdaExp
        | Apply LambdaExp LambdaExp
        | Variable String

lambda :: Parser LambdaExp
lambda = do
        char '\\'
        x <- many1 letter
        char '.'
        body <- lambdaExp
        return $ Lambda x body
        
apply :: Parser LambdaExp
apply = do
        char '('
        t1 <- lambdaExp
        skipMany1 space
        t2 <- lambdaExp
        char ')'
        return $ Apply t1 t2
        
variable :: Parser LambdaExp
variable = do
        x <- many1 letter
        return $ Variable x

lambdaExp :: Parser LambdaExp
lambdaExp = lambda <|> apply <|> variable

readExpr :: String -> Either ParseError LambdaExp
readExpr input = parse lambdaExp "lambda" input 