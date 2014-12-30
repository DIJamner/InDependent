module InDependentParser where

import DependentLambdaParser
import DependentLambda
import InDependent

import Text.Parsec
import Text.Parsec.String (Parser)

inde :: Parser InDependent
inde = many1 (do
        s <- statement
        char '\n'
        whitespace
        return s)

statement :: Parser Statement
statement = try native 
        <|> try adt
        <|> comment
        <|> inline
        <|> assign
        <?> "statement"
        
native :: Parser Statement
native = do
        string "native"
        spaces
        (v,e) <- typeAnnotation
        return $ Native v e
        
adt :: Parser Statement
adt = do
        string "data"
        spaces
        v <- var --TODO: change to typeAnnotation? for MyType : Type2
        spaces
        char '{'
        cons <- many1 $ try (do
                whitespace
                c <- typeAnnotation
                char '\n'
                return c)
        whitespace
        char '}'
        return $ ADT v cons

comment :: Parser Statement
comment = do
        string "--"
        comm <- many (satisfy (\c -> not (c == '\n')))
        return $ Comment comm

inline :: Parser Statement
inline = do
        string "{*"
        ls <- manyTill line (string "*}")
        return $ Inline $ foldr (++) "" ls

line :: Parser String
line = do
        text <- many (satisfy (\c -> not (c == '\n')))
        char '\n'
        return $ text ++ "\n"

whitespace :: Parser ()
whitespace = skipMany (newline <|> space)

assign :: Parser Statement
assign = do
        ta <- optionMaybe $ do
                ta <- try typeAnnotation
                char '\n'
                return ta
        v <- var
        spaces
        char '='
        spaces
        e <- expr
        case ta of
                Nothing -> return $ Assign v Nothing e
                Just (v1, t) -> if v1 == v then return $ Assign v (Just t) e
                        else fail $ "Type annotation for " ++ v1 ++
                                " requires an accompanying definition."
        

typeAnnotation :: Parser (String, Expr)
typeAnnotation = do
        v <- var
        spaces
        char ':'
        spaces
        e <- expr
        return (v, e)
        
        
        
        
        
        
        
