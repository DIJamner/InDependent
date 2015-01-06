module InDependent where

import DependentLambda
import qualified Errors as E

data Statement--TODO: add file imports
        = Assign String (Maybe Expr) Expr --TODO: make type mandatory and infer if not there?
        | Native String Expr 
        | ADT String Expr [(String,Expr)] --ADTName, Type, [(Constructor, ConsType)]
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
validateS :: RefEnv -> Int -> Int -> Statement -> E.ErrLineMonad RefEnv --TODO: ADT constructors should have to return the appropriate data type
--TODO: variables should only be defined once in a given scope. Should two definitions throw an error?
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
validateS re sp ep (ADT s t []) = return $ (s, t, Nothing):re
validateS re sp ep (ADT s t ((c, tt):cs)) = (do 
--TODO: Also, clean up a !!_LOT_!!
        --doesReturn tt --TODO: this should eventually be uncommented
        nt <- E.errWithLines sp ep $ normalize re [] tt
        newre <- validateS re sp ep (ADT s t cs)
        return $ (c, nt, Nothing):newre) where
                doesReturn :: Expr -> E.ErrLineMonad ()--TODO: massively oversimplified. Needs to account for Vec #1 Zero being a vec. Should include or no?
                doesReturn argType = case argType of
                        Var (Ref as) -> if as == s then return ()
                                else returnTypeErr argType
                        Pi req at rt -> doesReturn rt
                        _ -> returnTypeErr argType
                returnTypeErr at =  Left (sp, ep, E.TypeError $ show c ++ "must return a value of type " ++
                                s ++ " but returns a value of type " ++ show at)
validateS re sp ep (Inline s) = return re
validateS re sp ep (Comment s) = return re


