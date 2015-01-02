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
        newre <- case validateS re s of
                Left err -> Left (sp, ep, err)
                Right res -> return res
        validate newre ss

--attempts to validate a given statement in the current environment
--and returns a new environment if successful
validateS :: RefEnv -> Statement -> E.ErrMonad RefEnv --TODO: ADT constructors should return the appropriate data type
--TODO: variables should only be defined once in a given scope. SHould two definitions throw an error?
validateS re (Assign s mt e) = case mt of
        Nothing -> do
                ne <- normalize re [] e
                t <- nInferType re [] ne
                return $ (s, t, Just ne):re
        Just at -> do
                ne <- normalize re [] e
                nat <- normalize re [] at
                t <- nInferType re [] ne
                if nExprEq t nat then return $ (s, t, Just ne):re
                        else fail $ s ++ " should be of type " ++ show nat ++
                                " but is of type " ++ show t
validateS re (Native s t) = do
        nt <- normalize re [] t
        return $ (s, nt, Nothing):re
validateS re (ADT s []) = return $ (s, Universe 1, Nothing):re
validateS re (ADT s ((c, t):cs)) = do
         nt <- normalize re [] t
         newre <- validateS re (ADT s cs)
         return $ (c, nt, Nothing):newre
validateS re (Inline s) = return re
validateS re (Comment s) = return re