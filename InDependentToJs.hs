module InDependentToJS where

import InDependent
import DependentLambda
import qualified JSWriter as JS
import Errors as E

indeToJS :: RefEnv -> InDependent -> JS.JavaScript
indeToJS re [] = []
indeToJS re (s:ss) = (statementToJS re s):(indeToJS re ss) 

statementToJS :: RefEnv -> Statement -> JS.Statement --TODO: need refenv? errmonad?
statementToJS re (Assign s _ e) = JS.NewVar s $ lambdaToJS re [] e
statementToJS re arg@(Native _ _) = JS.Comment $ show arg
statementToJS re (ADT s cs) = genADTJS s cs
statementToJS re (Inline s) = JS.CodeBlock s
statementToJS re (Comment s) = JS.Comment s

genADTJS :: String -> [(String, Expr)] -> JS.Statement
genADTJS s cs = JS.StmntList (map (genConstructorJS s) cs)

genConstructorJS :: String -> (String, Expr) -> JS.Statement
genConstructorJS s (c, t) = JS.StmntList [
                genConsFuncJS s (c, t),
                genConsObjJS s (c, t)
        ]
        
genConsFuncJS :: String -> (String, Expr) -> JS.Statement
genConsFuncJS s (c, t) = JS.NewVar c $ JS.AnonymousFunction args (funcBody args)
        where
                args = (getArgStrings 1 t)
                getArgStrings :: Int -> Expr -> [String]
                getArgStrings i (Pi r at rt) = case r of
                        Expl -> ("$d" ++ show i):(getArgStrings (i+1) rt)
                        Impl -> getArgStrings i rt
                getArgStrings i _ = []
                funcBody :: [String] -> JS.JavaScript
                funcBody ss = [JS.Return $ JS.NewObj $ JS.FunctionCall (JS.Variable $ "$ADT" ++ c) 
                        (map JS.Variable args)]

genConsObjJS :: String -> (String, Expr) -> JS.Statement
genConsObjJS s (c, t) = JS.Function ("$ADT" ++ c) args (funcBody $ len)
        where
                (len, args) = (getArgStrings 1 t)
                getArgStrings :: Int -> Expr -> (Int,[String])
                getArgStrings i (Pi r at rt) = let 
                                (l, as) = (getArgStrings (i+1) rt) 
                        in case r of
                                Expl -> (l,("$d" ++ show i):as)
                                Impl -> getArgStrings i rt
                getArgStrings i _ = (i-1,[])
                funcBody :: Int -> JS.JavaScript
                funcBody i = (JS.Assignment ("this[0]") $ JS.LInt len):
                        (JS.Assignment ("this.isInDependent") $ JS.Variable "true"):
                        (map fieldAssign $ countFrom i)
                fieldAssign :: Int -> JS.Statement
                fieldAssign i = JS.Assignment ("this[" ++ show i ++"]") $ JS.Variable $
                         "$d" ++ show i
                countFrom :: Int -> [Int]
                countFrom 0 = []
                countFrom i = i:(countFrom $ i - 1)

lambdaToJS :: RefEnv -> IndexEnv -> Expr -> JS.Expr --TODO: use errmonad
lambdaToJS re ie (Var (Ref s)) = JS.Variable s 
lambdaToJS re ie arg@(Var (DeBruijn i)) = case resolveDeBruijn ie i of
        Right (Var (Ref s)) -> JS.Variable s --TODO: add error cases? Should never error if validated
        Left err -> JS.Variable $ "Uh oh. " ++ show err
        Right res -> JS.Variable $ "Uh oh. " ++ show res
lambdaToJS re ie (Universe i) = JS.FunctionCall (JS.Variable "Universe") [(JS.LInt i)] --TODO: use Object prototype to implement typesystem in JS????? IMPORTANT!!!!!!!!!!
lambdaToJS re ie (Pi r at rt) = JS.FunctionCall (JS.Variable "PiType") 
        [(lambdaToJS re ie at),(lambdaToJS re ie rt)]
lambdaToJS re ie (Lambda r t e) = let
                argname = "$d" ++ (show $ length ie)
                bodyJS = lambdaToJS re ((t, Just $ Var $ Ref argname):ie) e
        in case r of
                Impl -> bodyJS
                Expl -> JS.AnonymousFunction [argname] 
                        [JS.Return $ bodyJS]
lambdaToJS re ie (Apply a b) = case a of
        --if the argument is declared as implicit, then it should be ignored/inferred in the JS
        Lambda Impl t e -> lambdaToJS re ie e --TODO: works only if impl args are referenced only in the type. Need to check in validate
        Var (Ref s) -> case inferType re ie a of
                Right (Pi Impl at rt) -> lambdaToJS re ie a
                Right _ -> JS.FunctionCall (lambdaToJS re ie a) [(lambdaToJS re ie b)]
        Pi Impl at rt -> lambdaToJS re ie rt
        _ -> JS.FunctionCall (lambdaToJS re ie a) [(lambdaToJS re ie b)]