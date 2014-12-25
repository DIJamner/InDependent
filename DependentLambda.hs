module DependentLambda where
import qualified Errors as E
import Control.Monad
import Control.Applicative
--TODO: make sure that normalize is not called more than necessary. e.g. inferUniverse
data Expr
        = Var Variable
        | Universe Int
        | Pi Expr Expr --dependent product (pi _:t.e) We can extend de Bruijn notation to the dependent product type, as given \a:A.b : (Pi)a:A.B, the index is valid in both. TODO: This is true iff lambda terms cannot appear in valid types
        | Lambda Expr Expr --lambda abstraction (\_:t.e)
        | Apply Expr Expr --application (a b)

instance Show Expr where
        show (Var v) = show v
        show (Universe i) = "Type" ++ (show i)
        show (Pi t e) = (show t) ++ " -> " ++ (show e)
        show (Lambda t e) = "\\:(" ++ (show t) ++ ")." ++ (show e)
        show (Apply a b) = "(" ++ show a ++ " " ++ show b ++ ")"

data Variable
         = DeBruijn Int {-references bound variables from lambdas indexed by structural proximity.
                         in this program, de Bruijn indices start at 0 rather than 1 
                         to align with list indices-}
         | Ref String --references predefined terms bound to names in the environment
         deriving (Eq)
         
instance Show Variable where
        show (DeBruijn i) = "#" ++ show i
        show (Ref s) = "@" ++ s

type IndexEnv = [(Expr, Maybe Expr)] -- (_:t = e)successive terms should be concatenated at the end of the list, not the beginning
--using negative DeBruijn indices might be more efficient as list concatenation is much slower than appending

type RefEnv = [(String, Expr, Maybe Expr)]--(v:t = e)

--Attempts to calculate the type of the term in the provided environment
inferType :: RefEnv -> IndexEnv -> Expr -> E.ErrMonad Expr--TODO: merge RefEnv, IndexEnv into tuple?
inferType re ie e = (nInferType re ie) =<< (normalize re ie e)
        where nInferType re ie e = case e of
                Var v -> case v of
                        DeBruijn i -> deBruijnType ie i
                        Ref s -> refType re s
                Universe i -> Right $ Universe (i + 1)
                Pi t ee -> Universe `fmap` univMax --TODO: need to better understand
                        where
                                univMax :: E.ErrMonad Int
                                univMax = ((max `fmap` (inferUniverse re ie t)) <*> (inferUniverse re ie ee))
                Lambda t ee -> Pi t `fmap` (inferType re (ie ++ [(t, Nothing)]) ee)
                Apply a b -> do
                        at <- inferType re ie a 
                        bt <- inferType re ie b
                        case at of
                                Pi t e -> if exprEq re ie t bt then return $ deBruijnSub 1 b e
                                        else Left $ E.TypeError $ show b ++ " : " 
                                                ++ show bt ++ " not of type " ++ show t
                                _ -> Left $ E.CompilationError $ show a ++ 
                                        " : " ++ show at ++ " is not a function."
        
--attempts to determine the type of a type
inferUniverse :: RefEnv -> IndexEnv -> Expr -> E.ErrMonad Int
inferUniverse re ie e = inferUn' e
        where inferUn' ex = case ex of
                Universe i -> Right i
                t -> Left $ E.TypeError $ show t ++ " is not a universe."


normalize :: RefEnv -> IndexEnv -> Expr -> E.ErrMonad Expr
normalize re ie arg@(Var (DeBruijn i)) = return $ E.catch (\x -> arg) $ resolveDeBruijn ie i
normalize re ie arg@(Var (Ref s)) = return $ E.catch (\x -> arg) $ resolveRef re s
normalize re ie arg@(Universe i) = Right arg
normalize re ie (Pi t e) = Pi `fmap` (normalize re ie t) <*> (normalize re (ie ++ [(t,Nothing)]) e)
normalize re ie (Lambda t e) = Lambda `fmap` (normalize re ie t) <*> (normalize re (ie ++ [(t,Nothing)]) e) 
normalize re ie (Apply a b) = join $ (nApply re ie) `fmap` (normalize re ie a) `ap` (normalize re ie b) 
        where
                nApply :: RefEnv -> IndexEnv -> Expr -> Expr -> E.ErrMonad Expr
                nApply re ie a@(Lambda t e) b = normalize' =<< btype
                        where
                                btype :: E.ErrMonad Expr
                                btype = inferType re ie b
                                normalize' :: Expr -> E.ErrMonad Expr
                                normalize' bt  = if exprEq re ie bt t then Right $ deBruijnSub 1 b e
                                        else Left $ E.TypeError $ "expected " ++ show t ++ ", actual " ++ show btype ++ " in " ++
                                                show a ++ " applied to " ++ show b
                nApply re ie a b = return $ Apply a b

--replaces the given index (relative to the current scope) with the expression
--e.g. (1, e, \1) = e, (1, e, \\(2 \1)) = \(e \1)
deBruijnSub :: Int -> Expr -> Expr -> Expr
deBruijnSub i e arg@(Var (DeBruijn ii)) = if i == ii then e else arg
deBruijnSub i e arg@(Var (Ref _)) = arg
deBruijnSub i e arg@(Universe _) = arg
deBruijnSub i e (Pi t e2) = Pi (deBruijnSub i e t) (deBruijnSub (i+1) e e2)--TODO: does this work?
deBruijnSub i e (Lambda t e2) = Lambda (deBruijnSub i e t) (deBruijnSub (i+1) e e2)
deBruijnSub i e (Apply a b) = Apply (deBruijnSub i e a) (deBruijnSub i e b)

--replaces all instances of named reference with a given expression
refSub :: String -> Expr -> Expr -> Expr
refSub s e arg@(Var (DeBruijn _)) = arg
refSub s e arg@(Var (Ref r)) = if s == r then e else arg
refSub s e arg@(Universe _) = arg
refSub s e (Pi t e2) = Pi (refSub s e t) (refSub s e e2)
refSub s e (Lambda t e2) = Lambda (refSub s e t) (refSub s e e2)
refSub s e (Apply a b) = Apply (refSub s e a) (refSub s e b)

resolveDeBruijn :: IndexEnv -> Int -> E.ErrMonad Expr --TODO: combine with below for tuple output?
resolveDeBruijn ie i = if length ie >= i then case e of 
                Nothing -> Right $ Var $ DeBruijn i
                Just v -> Right $ v 
        else Left $ E.FreeVarError $ "Unresolved DeBruijn index " ++ (show i) ++ "."
        where (t, e) = ie !! (i-1) --de Bruijn indices are 1 indexed

deBruijnType :: IndexEnv -> Int -> E.ErrMonad Expr
deBruijnType ie i = if length ie >= i then return t
        else Left $ E.FreeVarError $ "Unresolved DeBruijn index " ++ (show i) ++ "."
        where (t, e) = ie !! (i-1) --de Bruijn indices are 1 indexed

resolveRef :: RefEnv -> String -> E.ErrMonad Expr
resolveRef [] s = Left $ E.FreeVarError $ "Type of " ++ s ++ " unknown."
resolveRef ((v, t, e):re) s = if s == v then case e of
                Nothing -> return $ Var $ Ref s
                Just ee -> return ee
        else Right =<< (resolveRef re s)
        
refType :: RefEnv -> String -> E.ErrMonad Expr
refType [] s = Left $ E.FreeVarError $ "Type of " ++ s ++ " unknown."
refType ((v, t, e):re) s = if s == v then Right t else refType re s

--tests if two expressions are equal
exprEq :: RefEnv -> IndexEnv -> Expr -> Expr -> Bool --TODO: should eat errors or no?
exprEq re ie a b = let
                nExprEq :: Expr -> Expr -> Bool --assumes both terms have been normalized
                nExprEq (Var a) (Var b) = a == b 
                nExprEq (Apply a1 b1) (Apply a2 b2) = nExprEq a1 a2 && nExprEq b1 b2
                nExprEq (Lambda t1 e1) (Lambda t2 e2) = nExprEq t1 t2 && nExprEq e1 e2
                nExprEq (Universe i1) (Universe i2) = i1 == i2
                nExprEq (Pi t1 e1) (Pi t2 e2) = nExprEq t1 t2 && nExprEq e1 e2
                nExprEq a b = False
        in case nExprEq `fmap` (normalize re ie a) <*> (normalize re ie b) of
                Left _ -> False
                Right res -> res

--performs either a lambda or pi abstraction of a variable over an expression
abstract :: (Expr -> Expr -> Expr) -> String -> Expr -> Expr -> Expr
abstract exprKind s t e = exprKind t $ abstract' 1 s e where
        --replaces all instances of the given variable with the appropriate de Bruijn index
        abstract' :: Int -> String -> Expr -> Expr
        abstract' i s arg@(Var (Ref ss)) = if s == ss then Var $ DeBruijn i else arg
        abstract' i s (Var (DeBruijn ii)) = Var $ DeBruijn (ii + 1)
        abstract' i s arg@(Universe ii) = arg
        abstract' i s (Pi t e) = Pi (abstract' i s t) (abstract' (i + 1) s e)
        abstract' i s (Lambda t e) = Lambda (abstract' i s t) (abstract' (i + 1) s e)
        abstract' i s (Apply a b) = Apply (abstract' i s a) (abstract' i s b)







