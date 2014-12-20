module Errors where
import Text.Parsec.String (Parser)
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
        
--lifts straight functions into error output functions
(^??) :: (b -> c) -> b -> Either a c
f ^?? e = Right $ f e

--lifts straight functions into error io functions, shorthand for ^?? and ??>> together
infixr 9 ^??>>
(^??>>) :: (b -> c) -> Either a b -> Either a c
f ^??>> e = (f ^??) ??>> e

--combinator for higher order error out functions
infixr 8 <??>
(<??>) :: (Either a (b -> c)) -> Either a b -> Either a c
f <??> e = either (\x -> Left x) (\x -> either (\y -> Left y) (\y -> Right (x y)) e) f

--lifts an error out function to an error io function
infixr 9 ??>>
(??>>) :: (b -> Either a c) -> Either a b -> Either a c
f ??>> e = either (\x -> Left x) (\x -> f x) e