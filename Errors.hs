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

type ErrMonad = Either Error


catch :: (Error -> a) -> ErrMonad a -> a
catch f e = case e of
        Left err -> f err
        Right res -> res

--lifts straight functions into error output functions
(^??) :: (b -> c) -> b -> ErrMonad c
f ^?? e = return $ f e

--lifts straight functions into error io functions, shorthand for ^?? and ??>> together
infixr 9 ^??>>
(^??>>) :: (b -> c) -> ErrMonad b -> ErrMonad c
f ^??>> e = (f ^??) ??>> e

--combinator for higher order error out functions
infixr 8 <??>
(<??>) :: (ErrMonad (b -> c)) -> ErrMonad b -> ErrMonad c
f <??> e = either (\x -> Left x) (\x -> either (\y -> Left y) (\y -> Right (x y)) e) f

--I think that positioning the arguments in the order that they would be if there were no errors/error monad makes more sense in the context of error handling.
infixr 9 ??>>
(??>>) :: (b -> ErrMonad c) -> ErrMonad b -> ErrMonad c
f ??>> e = e >>= f
