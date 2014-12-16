module TypedLambda where
import System.Environment
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Errors as E

data LambdaExp
        = Lambda String SimpleType LambdaExp
        | Apply LambdaExp LambdaExp
        | Variable String
        deriving (Eq)

instance Show LambdaExp where
        show (Variable s) = "var " ++ s
        show (Apply e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
        show (Lambda s t e) = "\\" ++ s ++ " : " ++ show t ++ "." ++ show e

data SimpleType 
        = Primitive String
        | Function SimpleType SimpleType
        deriving (Eq)   
        
instance Show SimpleType where
        show (Primitive s) = s
        show (Function t1 t2) = "(" ++ (show t1) ++ " -> " ++ (show t2) ++ ")"

type Environment = [(String, SimpleType)]

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
                t <- primitiveType prims <|> (parens $ typeExp prims)
                ts <- many $ (string "->") >> (typeExp prims)
                return $ typeListToFunc (t:ts)

typeListToFunc :: [SimpleType] -> SimpleType
typeListToFunc (t:[]) = t
typeListToFunc (t:ts) = Function t (typeListToFunc ts)

lambdaExp :: [String] -> Parser LambdaExp
lambdaExp prims = (lambda prims) <|> (apply prims) <|> variable

--attempts to calculate the type of the expression with a given environment     
getType :: LambdaExp -> Environment -> Either E.Error SimpleType
getType (Variable x) [] = Left $ E.TypeError $ "Variable " ++ x ++ " is free."
getType a@(Variable x) ((y, t):ys) = if x == y then Right t else getType a ys
getType (Lambda x t body) e = case getType body ((x, t):e) of
        Left err -> Left err
        Right tt -> Right $ Function t tt
getType (Apply a b) e = case getType a e of
        Left err -> Left err
        Right tt -> case tt of
                Primitive x -> Left $ E.TypeError $ show a ++ " of type " ++ x ++ " is not a function."
                Function t1 t2 -> case getType b e of
                        er@(Left err) -> er
                        Right t -> if t == t1 then Right t2
                                else Left $ E.TypeError $ "Function " ++ show a ++ 
                                        " required an argument of type " ++ show t1 ++ " but " ++ show b ++
                                        " was of type " ++ show t

readExpr :: String -> [String] -> Either ParseError LambdaExp
readExpr input prims = parse (lambdaExp prims) "typed lambda" input