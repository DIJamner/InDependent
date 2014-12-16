import TypedLambda
import qualified JSWriter as JS
import qualified Errors as E
import Text.Parsec.String (Parser)
import Text.Parsec
import System.Environment
import Data.Either
import Data.Map (Map)
import qualified Data.Map as Map
--TODO: Document
type DustyLang = [DStatement]

data DStatement 
        = DAssign String LambdaExp
        | DExp LambdaExp
        | DNative String SimpleType
        | DComment String
--TODO: how to do numeric primitives? builtins?
primTypes = ["Int"]


--parses a block of DustyLang code
dustyParser :: Parser DustyLang
dustyParser = many1 $ do--TODO: add comments
        a <- comment <|> declNative <|> assignment <|> expStatement
        ((skipMany1 $ char '\n') <|> eof)
        return $ a

declNative :: Parser DStatement
declNative = do
        string "native"
        spaces
        name <- varIdentifier
        spaces
        char ':'
        spaces
        objType <- typeExp primTypes--TODO: eventually will have user-defined types and will need to pass a type environment (actually, scratch that; one environment will contain both (dependent types))
        return $ DNative name objType

comment :: Parser DStatement --TODO: single line comments
comment = do
        string "{-"
        s <- manyTill anyChar (try (string "-}"))
        return $ DComment s

expStatement :: Parser DStatement
expStatement = do
        e <- lambdaExp primTypes
        return $ DExp e

--creates an assignment parser from an a parser
assignment :: Parser DStatement
assignment = do
        name <- varIdentifier
        spaces
        char '='
        spaces
        value <- lambdaExp primTypes
        return $ DAssign name value


--Attempts to generate JS from DustyLang and returns a tuple
--containing a list of errors and the resultant JS.
--If there are errors the generated JS is not guarenteed to be correct.
genJS :: DustyLang -> ([E.Error], JS.JavaScript)
genJS expr = partitionEithers $ mapInEnvironment processStmnt expr []

--general function
mapInEnvironment :: (a -> b -> (b, c)) -> [a] -> b -> [c]
mapInEnvironment f [] b = []
mapInEnvironment f (a:as) b =  c:(mapInEnvironment f as newB) where (newB, c) = f a b

processStmnt :: DStatement -> Environment -> (Environment, Either E.Error JS.Statement)
processStmnt (DAssign n v) e = case getType v e of
--TODO: func. args. currently pass out of their scope in the type checker
        Left err -> (e, Left err)
        Right t -> ((n, t):e, Right $ JS.NewVar n $ genExp v)
processStmnt (DExp expr) e = case getType expr e of
        Left err -> (e, Left err)
        Right t -> (e, Right $ JS.StateExp $ genExp expr)
processStmnt (DComment s) e = (e, Right $ JS.Comment s)
processStmnt (DNative s t) e = ((s,t):e, Right $ JS.Comment $ "native " ++ s ++ " : " ++ show t)

genExp :: LambdaExp -> JS.JSExp
genExp (Variable s) = JS.Variable s
genExp (Lambda x _ body) = JS.AnonymousFunction [x] [JS.Return $ genExp body]
genExp (Apply a b) = JS.FunctionCall (genExp a) [genExp b]

--attempts to parse a string of dustylang code
--and either returns an error or the result
readDusty :: String  -> Either ParseError DustyLang
readDusty input = parse dustyParser "dustylang" input

--attempts to compile a string of dustylang code
--and outputs a tuple containing a list of errors
--and a generated JS code string
compile :: String -> ([E.Error], String)
compile s = case readDusty s of
        Left err -> ([E.ParsecError err],"")
        Right d -> (errs, text) where
                (errs, gen) = genJS d
                text = JS.toText 0 $ gen

main :: IO ()
main = do
        args <- getArgs
        if (args !! 0) !! 0 == '-' then case args !! 0 of
                        "-type" -> case readExpr (args !! 1) primTypes of
                                Left err -> putStrLn $ "Failed due to " ++ show err
                                Right l -> putStrLn $ show (getType l [])
                        "-i" -> do
                                src <- readFile (args !! 1)
                                let (errs, bin) = compile src
                                putStrLn $ "errors:\n" ++ show errs
                                putStrLn $ "output:\n" ++ bin
                                
                else do
                        let (errs, bin) = compile (args !! 0)
                        putStrLn $ "errors:\n" ++ show errs
                        putStrLn $ "output:\n" ++ bin
                        