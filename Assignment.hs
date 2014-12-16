module Assignment where
import Text.Parsec.String (Parser)
import Text.Parsec

--Represents a variable binding
data Assignment a = Assign String a

--creates an assignment parser from an a parser
assignment :: Parser a -> Parser (Assignment a)
assignment p = do
        name <- many1 letter
        spaces
        char '='
        spaces
        value <- p
        return $ Assign name value