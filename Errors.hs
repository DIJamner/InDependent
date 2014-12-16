module Errors where
import Text.Parsec.String (Parser)
import Text.Parsec
--TODO: use packaging

--This ADT encapsulates all types of errors that can be thrown by the compiler
data Error 
        = TypeError String
        | ParsecError ParseError
        | CompilationError String
        deriving (Show)
        
--This function converts an error into a string message for adding to another error's message
--TODO: should errors be nestable, removing the need for this?
message :: Error -> String
message (TypeError s) = "TypeError: " ++ s
message (ParsecError p) = show p
message (CompilationError s) = "CompilationError: " ++ s