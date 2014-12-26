module DustyToJS where

import Dusty
import DependentLambda
import qualified JSWriter as JS
import Errors as E

dustyToJS :: Dusty -> JS.JavaScript
dustyToJS [] = []
dustyToJS (s:ss) = (statementToJS s):(dustyToJS ss) 

statementToJS :: Statement -> JS.Statement --TODO: need refenv? errmonad?
statementToJS (Assign s _ e) = JS.Assignment s $ lambdaToJS [] e
statementToJS arg@(Native _ _) = JS.Comment $ show arg
statementToJS (ADT s cs) = genADTJS s cs
statementToJS (Inline s) = JS.CodeBlock s
statementToJS (Comment s) = JS.Comment s

genADTJS :: String -> [(String, Expr)] -> JS.Statement
genADTJS s cs = JS.StmntList (map (genConstructorJS s) cs)

genConstructorJS :: String -> (String, Expr) -> JS.Statement
genConstructorJS s (c, t) = JS.StmntList [
                genConsFuncJS s (c, t),
                genConsObjJS s (c, t)
        ]
        
genConsFuncJS :: String -> (String, Expr) -> JS.Statement
genConsFuncJS s (c, t) = JS.Assignment c $ JS.AnonymousFunction args (funcBody args)
        where
                args = (getArgStrings 1 t)
                getArgStrings :: Int -> Expr -> [String]
                getArgStrings i (Pi at rt) = ("$d" ++ show i):(getArgStrings (i+1) rt)
                getArgStrings i _ = []
                funcBody :: [String] -> JS.JavaScript
                funcBody ss = [JS.Return $ JS.NewObj $ JS.FunctionCall (JS.Variable $ "$ADT" ++ c) 
                        (map JS.Variable args)]

genConsObjJS :: String -> (String, Expr) -> JS.Statement
genConsObjJS s (c, t) = JS.Function ("$ADT" ++ c) args (funcBody args)
        where
                args = (getArgStrings 1 t)
                getArgStrings :: Int -> Expr -> [String]
                getArgStrings i (Pi at rt) = ("$d" ++ show i):(getArgStrings (i+1) rt)
                getArgStrings i _ = []
                funcBody :: [String] -> JS.JavaScript
                funcBody ss = (map (\v -> JS.Assignment ("this." ++ v) $ JS.Variable v) ss)--TODO: add type? Necessary? How to deal with rec. of new? return?,


lambdaToJS :: IndexEnv -> Expr -> JS.Expr
lambdaToJS ie (Var (Ref s)) = JS.Variable s 
lambdaToJS ie arg@(Var (DeBruijn i)) = case resolveDeBruijn ie i of
        --TODO: debruijn is not being subbed
        Right (Var (Ref s)) -> JS.Variable s --TODO: add error cases? Should never error if validated
        Left err -> JS.Variable $ "Uh oh. " ++ show err
        Right res -> JS.Variable $ "Uh oh. " ++ show res
lambdaToJS ie (Universe i) = JS.FunctionCall (JS.Variable "Universe") [(JS.LInt i)] --TODO: use Object prototype to implement typesystem in JS????? IMPORTANT!!!!!!!!!!
lambdaToJS ie (Pi at rt) = JS.FunctionCall (JS.Variable "PiType") 
        [(lambdaToJS ie at),(lambdaToJS ie rt)]
lambdaToJS ie (Lambda t e) = JS.AnonymousFunction [argname] 
        [JS.Return $ lambdaToJS ((t, Just $ Var $ Ref argname):ie) e]
        where argname = "$d" ++ (show $ length ie)
lambdaToJS ie (Apply a b) = JS.FunctionCall (lambdaToJS ie a) [(lambdaToJS ie b)]