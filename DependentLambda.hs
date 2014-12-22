module DependentLambda where
import Errors ((^??), (^??>>), (??>>), (<??>))
import qualified Errors as E

data Expr
        = Var Variable
        | Universe Int
        | Pi Expr Expr --dependent product (pi _:t.e) TODO: BIG: does DeBruijn notation extend to dependent products
        | Lambda Expr Expr --lambda abstraction (\_:t.e)
        | Apply Expr Expr --application (a b)
        deriving (Show)

data Variable
         = DeBruijn Int {-references bound variables from lambdas indexed by structural proximity.
                         in this program, de Bruijn indices start at 0 rather than 1 
                         to align with list indices-}
         | Ref String --references predefined terms bound to names in the environment
         deriving (Show, Eq)
    
type IndexEnv = [Maybe Expr] --successive terms should be concatenated at the end of the list, not the beginning
--using negative DeBruijn indices might be more efficient as list concatenation is much slower than appending

type RefEnv = [(String, Expr, Maybe Expr)]--(v:t = e)

--Attempts to calculate the type of the term in the provided environment
inferType :: RefEnv -> IndexEnv -> Expr -> E.ErrMonad Expr--TODO: merge RefEnv, IndexEnv into tuple?
inferType re ie e = (nInferType re ie) ??>> (normalize re ie e)
        where nInferType re ie e = case e of --TODO: inferType is the only function that uses ie. if there a way to remove it entirely?
                Var v -> case v of
                        DeBruijn i -> (inferType re ie) ??>> (resolveDeBruijn ie i)--TODO: right?
                        Ref s -> getType re s where --this could be replaced by an evaluation of the value of s, but since we have the type, checking it is more efficient.
                                getType :: RefEnv -> String -> E.ErrMonad Expr
                                getType [] s = Left $ E.FreeVarError $ "Type of " ++ s ++ " unknown."
                                getType ((v, t, e):re) s = if s == v then Right t else getType re s
                Universe i -> Right $ Universe (i + 1)
                Pi t ee -> Universe ^??>> univMax --TODO: need to better understand
                        where
                                univMax :: E.ErrMonad Int
                                univMax = ((max ^??>> (inferUniverse re ie t)) <??> (inferUniverse re ie ee))
                Lambda t ee -> Pi t ^??>> (inferType re (ie ++ [Just t]) ee)
                Apply a@(Lambda t ee) b -> (inferPi re ie b) ??>> (inferType re ie a) 
                Apply a _ -> Left $ E.TypeError $ show a ++ " is not a function."

--attempts to determine the type of a lmabda term
inferPi :: RefEnv -> IndexEnv -> Expr -> Expr -> E.ErrMonad Expr
inferPi re ie b e = inferPi' ??>> (normalize re ie e)
        where
                inferPi' :: Expr -> E.ErrMonad Expr
                inferPi' nf = case nf of
                        Pi tt ee -> checkPi ??>> btype
                                where 
                                        btype :: E.ErrMonad Expr
                                        btype = inferType re ie b
                                        checkPi :: Expr -> Either E.Error Expr
                                        checkPi bt = if exprEq re ie tt bt then Right $ deBruijnSub 1 tt ee
                                        else Left $ E.TypeError $ show b ++ " not of type " ++ show tt
                        _ -> Left $ E.CompilationError "Something strange happened."
        
--attempts to determine the type of a type
inferUniverse :: RefEnv -> IndexEnv -> Expr -> E.ErrMonad Int
inferUniverse re ie e = inferUn' ??>> (normalize re ie e)
        where inferUn' ex = case ex of
                Universe i -> Right i
                t -> Left $ E.TypeError $ show t ++ " is not a universe."


normalize :: RefEnv -> IndexEnv -> Expr -> E.ErrMonad Expr --TODO: what is the overlap w/ DeBruijnSub? DeBruijnSub is likely 1 normalize step
normalize re ie arg@(Var (DeBruijn i)) = return $ E.catch (\x -> arg) $ resolveDeBruijn ie i
normalize re ie arg@(Var (Ref s)) = return $ E.catch (\x -> arg) $ resolveRef re s
normalize re ie arg@(Universe i) = Right arg
normalize re ie (Pi t e) = Pi ^??>> (normalize re ie t) <??> (normalize re (ie ++ [Nothing]) e) --TODO: does this dummy val work?
normalize re ie (Lambda t e) = Lambda ^??>> (normalize re ie t) <??> (normalize re (ie ++ [Nothing]) e) --TODO: same as above
normalize re ie (Apply a@(Lambda t e) b) = normalize' ??>> btype
        where
                btype :: E.ErrMonad Expr
                btype = inferType re ie b
                normalize' :: Expr -> E.ErrMonad Expr
                normalize' bt  = if exprEq re ie bt t then Right $ deBruijnSub 1 b e
                        else Left $ E.TypeError $ "expected " ++ show t ++ ", actual " ++ show btype ++ " in " ++
                                show a ++ " applied to " ++ show b
normalize re ie (Apply a b) = Apply ^??>> (normalize re ie a) <??> (normalize re ie b)
        

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

resolveDeBruijn :: IndexEnv -> Int -> E.ErrMonad Expr
resolveDeBruijn ie i = if length ie >= i then case ie !! (i-1) of --de Bruijn indices are 1 indexed
                Nothing -> Right $ Var $ DeBruijn i
                Just v -> Right $ v 
        else Left $ E.FreeVarError $ "Unresolved DeBruijn index " ++ (show i) ++ "."

resolveRef :: RefEnv -> String -> E.ErrMonad Expr
resolveRef [] s = Left $ E.FreeVarError $ "Type of " ++ s ++ " unknown."
resolveRef ((v, t, e):re) s = if s == v then case e of
                Nothing -> return $ Var $ Ref s --TODO: could this lead to inf. loops?
                Just ee -> return ee
        else Right ??>> (resolveRef re s)

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
        in case nExprEq ^??>> (normalize re ie a) <??> (normalize re ie b) of
                Left _ -> False
                Right res -> res

