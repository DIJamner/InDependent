module Errors where
import Text.Parsec
--TODO: use packaging

--This ADT encapsulates all types of errors that can be thrown by the compiler
data Error 
        = TypeError String
        | ParsecError ParseError
        | CompilationError String
        | FreeVarError String
        | BindingError String
        deriving (Show)

type ErrMonad = Either Error

type Lined a = (Int, Int, a) --start line, end line, value

type ErrLineMonad = Either (Lined Error)

catch :: (b -> a) -> Either b a -> a
catch f e = case e of
        Left err -> f err
        Right res -> res
        
errWithLines :: Int -> Int -> ErrMonad a -> ErrLineMonad a
errWithLines sp ep (Right res) = Right res
errWithLines sp ep (Left err) = Left (sp, ep, err)
