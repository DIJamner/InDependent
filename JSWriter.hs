module JSWriter (JavaScript, JSExp(..), Statement(..), toText) where
--TODO: lots, incl. objects

type JavaScript = [Statement]

data JSExp
        = LString String
        | LInt Int
        | LDouble Double
        | Variable String
        | FunctionCall JSExp [JSExp]--TODO: what about calling anonymous functions? fn(){return 0;}()
        | BinaryOp JSExp String JSExp
        | UnaryOp JSExp String
        | ArrayElement JSExp Int
        | AnonymousFunction [String] JavaScript
        deriving (Eq, Show)
        
data Statement 
        = StateExp JSExp
        | NewVar String JSExp
        | Assignment String JSExp
        | If JSExp JavaScript --do I want to force blocks? yes, I think
        | Else JavaScript
        | For JavaScript JSExp JSExp JavaScript
        | Return JSExp
        | Comment String
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
stmntToText n s = ""

indent :: Int -> String
indent 0 = ""
indent n = "    " ++ indent (n - 1)

exprToText :: Int -> JSExp -> String
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