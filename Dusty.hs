module Dusty where

import DependentLambda
import qualified Errors as E

data Statement--TODO: add file imports
        = Assign String (Maybe Expr) Expr
        | Native String Expr 
        | ADT String [(String,Expr)] --Type [(Constructor, ConsType)]
        | Inline String --a section of target language code inlined in the source file
        | Comment String
        deriving(Show)
        
type Dusty = [Statement]

validate :: RefEnv -> Dusty -> E.ErrLineMonad ()
validate re code = validate' 1 re code where
        validate' :: Int -> RefEnv -> Dusty -> E.ErrLineMonad ()
        validate' l re [] = return ()
        validate' l re (s:ss) = do
                newre <- case validateS re s of
                        Left err -> Left (l, err)
                        Right res -> return res
                validate' (l+1) newre ss
                return ()

--attempts to validate a given statement in the current environment
--and returns a new environment if successful
validateS :: RefEnv -> Statement -> E.ErrMonad RefEnv
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