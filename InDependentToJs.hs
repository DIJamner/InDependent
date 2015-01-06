module InDependentToJS where

import InDependent
import DependentLambda
import qualified JSWriter as JS
import Errors as E

indeToJS :: RefEnv -> LinedInDependent -> JS.JavaScript
indeToJS re [] = []
indeToJS re ((sp, ep, s):ss) = (statementToJS re s):(indeToJS re ss) 

statementToJS :: RefEnv -> Statement -> JS.Statement --TODO: need refenv? errmonad?
statementToJS re (Assign s _ e) = JS.NewVar s $ lambdaToJS re [] e
statementToJS re arg@(Native _ _) = JS.Comment $ show arg
statementToJS re (ADT s t cs) = genADTJS re s t cs
statementToJS re (Inline s) = JS.CodeBlock s
statementToJS re (Comment s) = JS.Comment s

--TODO: restructure helper functions for clarity/increased modularity
genADTJS :: RefEnv -> String -> Expr -> [(String, Expr)] -> JS.Statement
genADTJS re s t cs = JS.StmntList $ (genConstructorJS re t (s,t)) 
        :(map (genConstructorJS re t) cs)--TODO: the use of t here  t has to be wrong...|_|\_||_|_|_|\-|_\ HERE HERE HERE HERE HERE HERE



genConstructorJS :: RefEnv -> Expr -> (String, Expr) -> JS.Statement
genConstructorJS re typeExpr (c, t) = JS.StmntList [
                genConsFuncJS c len args,
                genConsObjJS re typeExpr (c, t) len args ats
        ]  where (len, args, ats) = (getArgStrings 1 t)
        
genConsFuncJS :: String -> Int -> [String] -> JS.Statement        
genConsFuncJS c len args = JS.NewVar c $ if args == [] 
        then constrCall
        else JS.AnonymousFunction args funcBody
        where
                funcBody :: JS.JavaScript
                funcBody  = [JS.Return $ constrCall]
                constrCall = JS.NewObj $ JS.FunctionCall (JS.Variable $ "$ADT" ++ c) 
                        (map JS.Variable args)

genConsObjJS :: RefEnv -> Expr -> (String, Expr) -> Int -> [String] -> [Expr] -> JS.Statement
genConsObjJS re typeExpr (c, t) len args ats = JS.Function ("$ADT" ++ c) args (funcBody)
        where
                funcBody :: JS.JavaScript
                funcBody = (JS.Assignment ("this[0]") $ JS.LInt len):
                        (JS.Assignment ("this.inDeType") $ jsTypeExpr ):
                        (map fieldAssign $ countFrom len)
                        
                fieldAssign :: Int -> JS.Statement
                fieldAssign i = JS.Assignment ("this[" ++ show i ++"]") $ JS.Variable $
                         "$d" ++ show i
                         
                countFrom :: Int -> [Int]
                countFrom 0 = []
                countFrom i = i:(countFrom $ i - 1)
                
                exprArgs = map (\s -> Var $ Ref s) args--TODO: check if ats in right order or reversed, I think reversed
                
                subPi :: Int -> Expr -> Expr
                subPi i expr = case expr of
                        Pi r at rt -> Pi r at $ subPi (i+1) $ deBruijnSub 1 (exprArgs !! i) rt
                        _ -> expr
                
                jsTypeExpr = lambdaToJS re [] $ subPi 0 t
getArgStrings :: Int -> Expr -> (Int,[String],[Expr])
getArgStrings i (Pi r at rt) = let 
                (l, as, ts) = (getArgStrings (i+1) rt) 
        in case r of
                Expl -> (l,("$d" ++ show i):as, at:ts)
                Impl -> getArgStrings i rt
getArgStrings i _ = (i-1,[],[])

lambdaToJS :: RefEnv -> IndexEnv -> Expr -> JS.Expr --TODO: use errmonad
lambdaToJS re ie (Var (Ref s)) = JS.Variable s 
lambdaToJS re ie arg@(Var (DeBruijn i)) = case resolveDeBruijn ie i of
        Right (Var (Ref s)) -> JS.Variable s --TODO: add error cases? Should never error if validated
        Left err -> JS.Variable $ "Uh oh. " ++ show err
        Right res -> JS.Variable $ "Uh oh. " ++ show res
lambdaToJS re ie (Universe i) = JS.FunctionCall (JS.Variable "Universe") [(JS.LInt i)]
lambdaToJS re ie (Pi r at rt) = JS.FunctionCall (JS.Variable "PiType") --TODO: does not take into account deBruijn indices. What does this behavior affect?
        [(lambdaToJS re ie at),(lambdaToJS re ie rt)]
lambdaToJS re ie (Lambda r t e) = let
                argname = "$d" ++ (show $ length ie + 1)
                bodyJS = lambdaToJS re ((t, Just $ Var $ Ref argname):ie) e
        in case r of
                Impl -> bodyJS
                Expl -> JS.AnonymousFunction [argname] 
                        [JS.Return $ bodyJS]
lambdaToJS re ie (Apply a b) = case a of
        Lambda Impl t e -> lambdaToJS re ie e --TODO: remove impl args? do not work until can be inferred by type checker. Compiler uses types, so cannot just be compiler-implicit
        Var (Ref s) -> case inferType re ie a of
                Right (Pi Impl at rt) -> lambdaToJS re ie a
                Right _ -> JS.FunctionCall (lambdaToJS re ie a) [(lambdaToJS re ie b)]
        Pi Impl at rt -> lambdaToJS re ie rt
        _ -> JS.FunctionCall (lambdaToJS re ie a) [(lambdaToJS re ie b)]