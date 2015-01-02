module InDependent where

import DependentLambda
import qualified Errors as E

data Statement--TODO: add file imports
        = Assign String (Maybe Expr) Expr --TODO: make type mandatory and infer if not there?
        | Native String Expr 
        | ADT String [(String,Expr)] --Type [(Constructor, ConsType)] TODO: allow data MyType : Type2?
        | Inline String --a section of target language code inlined in the source file
        | Comment String
        deriving(Show)
        
type InDependent = [Statement]
        
type LinedInDependent = [E.Lined Statement]


validate :: RefEnv -> LinedInDependent -> E.ErrLineMonad RefEnv 
validate re [] = return re
validate re ((sp, ep, s):ss) = do
        newre <- validateS re sp ep s
        validate newre ss

--attempts to validate a given statement in the current environment
--and returns a new environment if successful
validateS :: RefEnv -> Int -> Int -> Statement -> E.ErrLineMonad RefEnv --TODO: ADT constructors should return the appropriate data type
--TODO: variables should only be defined once in a given scope. SHould two definitions throw an error?
validateS re sp ep (Assign s mt e) = case mt of
        Nothing -> do
                ne <- E.errWithLines sp ep $ normalize re [] e
                t <- E.errWithLines sp ep $ nInferType re [] ne
                return $ (s, t, Just ne):re
        Just at -> do
                ne <- E.errWithLines sp ep $ normalize re [] e
                nat <- E.errWithLines sp ep $  normalize re [] at
                t <- E.errWithLines sp ep $ nInferType re [] ne
                if nExprEq t nat then return $ (s, t, Just ne):re
                        else Left $ (sp, ep, E.TypeError $ s ++ " should be of type " ++ show nat ++
                               " but is of type " ++ show t)
validateS re sp ep (Native s t) = do
        nt <- E.errWithLines sp ep $ normalize re [] t
        return $ (s, nt, Nothing):re
validateS re sp ep (ADT s []) = return $ (s, Universe 1, Nothing):re
validateS re sp ep (ADT s ((c, t):cs)) = do
         nt <- E.errWithLines sp ep $ normalize re [] t
         newre <- validateS re sp ep (ADT s cs)
         return $ (c, nt, Nothing):newre
validateS re sp ep (Inline s) = return re
validateS re sp ep (Comment s) = return re