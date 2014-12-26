module JSWriter (JavaScript, Expr(..), Statement(..), toText) where --TODO: divide into JS and JSWriter
--TODO: lots, incl. objects

type JavaScript = [Statement]

data Expr
        = LString String
        | LInt Int
        | LDouble Double
        | Variable String
        | FunctionCall Expr [Expr]--TODO: what about calling anonymous functions? fn(){return 0;}()
        | BinaryOp Expr String Expr
        | UnaryOp Expr String
        | ArrayElement Expr Int
        | AnonymousFunction [String] JavaScript
        deriving (Eq, Show)
        
data Statement 
        = StateExp Expr
        | NewVar String Expr
        | Assignment String Expr
        | Function String [String] JavaScript
        | If Expr JavaScript
        | Else JavaScript
        | For JavaScript Expr Expr JavaScript
        | Return Expr
        | Comment String
        | CodeBlock String --represents user-written JS
        | StmntList [Statement]
        deriving (Eq, Show)

toText :: Int -> JavaScript -> String
toText indent js = foldl (++) "" (map (stmntToText indent) js)
        
stmntToText :: Int -> Statement -> String
stmntToText n (StateExp e) = indent n ++ exprToText n e ++ "\n"
stmntToText n (NewVar v expr) = indent n ++ "var " ++ v ++ " = " ++ exprToText n expr ++ "\n"
stmntToText n (Assignment v expr) = indent n ++ v ++ " = " ++ exprToText n expr ++ "\n"
stmntToText n (If expr js) = indent n ++ "if(" ++ exprToText n expr ++ "){\n" 
        ++ toText (n+1) js ++ "}\n"
        
stmntToText n (Return expr) = indent n ++ "return " ++ exprToText n expr ++ "\n"
stmntToText n (Comment s) = indent n ++ "/*" ++ s ++ "*/\n"
stmntToText n (CodeBlock s) = s ++ "\n"
stmntToText n (StmntList []) = ""
stmntToText n (StmntList (s:ss)) = (stmntToText n s) ++ (stmntToText n (StmntList ss))
stmntToText n (Function s [] js) = indent n ++ "function " ++ s ++ "(){\n" ++ toText (n+1) js ++ indent n ++ "}\n"
stmntToText n (Function s (arg:[]) js) = indent n ++ "function " ++ s ++ "(" ++ arg ++ "){\n" ++ toText (n+1) js ++ indent n ++ "}\n" 
stmntToText n (Function s (arg:args) js) = indent n ++ "function " ++ s ++ "(" ++ (foldr (\a b -> a ++ "," ++ b) arg args) ++ "){\n" ++ toText (n+1) js ++ indent n ++ "}\n" --TODO: make sure foldr is right, add js
stmntToText n s = ""--TODO: should eventually remove

indent :: Int -> String
indent 0 = ""
indent n = "    " ++ indent (n - 1)

exprToText :: Int -> Expr -> String
exprToText i (LString s) = "\"" ++ s ++ "\""
exprToText i (LInt int) = show int
exprToText i (LDouble d) = show d
exprToText i (Variable v) = v
exprToText i (FunctionCall fn es) = exprToText i fn ++ funcArgs (exprToText i) es
exprToText i (BinaryOp a op b) = exprToText i a ++ " " ++ op ++ " " ++ exprToText i b

exprToText i (AnonymousFunction ps js) = "function" ++ funcArgs (\s -> s) ps ++ "{\n"
        ++ toText (i + 1) js ++ indent i ++ "}"
exprToText i e = ""
        
funcArgs :: (a -> String) -> [a] -> String
funcArgs fn [] = "()"
funcArgs fn (e:es) = "(" ++ (foldl (++) (fn e) (map (\s -> ", " ++ fn s) es)) ++ ")"




test :: JavaScript
test = [
        NewVar "name" $ LString "MyName",
        If (BinaryOp (Variable "name") "===" $ LString "MyName") [
                StateExp $ FunctionCall (Variable "alert")[LString "You need a name."]
        ]
        
        ]

main :: IO ()
main = do
        putStrLn $ "//generated with JSWriter\n" ++ toText 0 test