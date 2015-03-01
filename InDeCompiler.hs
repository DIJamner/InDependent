import DependentLambda
import DependentLambdaParser
import InDependentParser
import qualified ImportParser as IP
import InDependent
import InDependentToJS
import InDependentToLLVM
import qualified Errors as E
import qualified JSWriter as JS

import System.Environment
import Text.Parsec

main :: IO ()
main = do 
        args <- getArgs
        case args !! 0 of
                "expr" -> processExpr args
                _ -> processInDependent 0 args print


processExpr :: [String] -> IO ()
processExpr args = case args !! 1 of
                "parse" -> do
                        putStrLn $ "Parsing " ++ (args !! 2)
                        print $ parse expr "lambdapi" $ args !! 2
                "normalize" -> print $ (normalize [] []) `fmap` (parse expr "lambdapi" $ args !! 2)
                "infertype" -> print $ (inferType [] []) `fmap` (parse expr "lambdapi" $ args !! 2)
                
processInDependent :: Int -> [String] -> (E.ErrLineMonad String -> IO ()) -> IO () --TODO: redo this function for E.Lined
processInDependent i args out = case args !! i of
        "-i" -> do
                src <- readFile (args !! (i+1))
                processInDependent (i+2) (args ++ [src]) out
        "-o" -> processInDependent (i+2) args (writeCode (args !! (i+1)))
        "validate" -> do
                let code = case parse inde "inde" (args !! (i + 1)) of
                        Left err -> Left (pos, pos, E.ParsecError err)
                                where pos = sourceLine $ errorPos err
                        Right res -> return res
                let valid = (validate []) =<< code
                putStrLn "Validate: "
                print code
                putStrLn "\nResult: "
                case valid of
                     Left err -> print err
                     Right _ ->  putStrLn "Valid!"
        "print" -> print $ parse inde "inde" (args !! (i + 1))
        
        "compile" -> compileFromArgs i args out
        a -> print $ "Compiler does not recognize flag: " ++ show a

compileFromArgs :: Int -> [String] -> (E.ErrLineMonad String -> IO ()) -> IO ()
compileFromArgs i args out = do
        let dcode = case parse inde "inde" (args !! (i + 1)) of
                Left err -> Left (pos, pos, E.ParsecError err)
                        where pos = sourceLine $ errorPos err
                Right res -> return res
        let valid = (validate []) =<< dcode
        case valid of
                Left err -> print err
                Right re -> do
                        let jscode = (indeToJS re) `fmap` dcode
                        out ((JS.toText 0) `fmap` jscode)

compile :: RefEnv -> [String] -> String -> (E.ErrLineMonad String -> IO ()) -> IO RefEnv
compile re past path out = do
        src <- readFile path
        let parsedata = case parse IP.inde "inde" src of
                Left err -> Left (pos, pos, E.ParsecError err)
                        where pos = sourceLine $ errorPos err
                Right res -> return res
        let dcode = snd `fmap` parsedata
        let imports = fst `fmap` parsedata
        nre <- case imports of
                Left err -> return []
                Right res -> compileImports re past res out
        let valid = (validate nre) =<< dcode
        case valid of
                Left err -> (print err)>>
                        (return nre)
                Right re -> do
                        let jscode = (indeToJS re) `fmap` dcode
                        out ((JS.toText 0) `fmap` jscode)
                        return re

compileImports :: RefEnv -> [String] -> [String] -> (E.ErrLineMonad String -> IO ()) -> IO RefEnv --TODO: past does not carry on correctly(will cause duplicate compilations but no if. loops)
compileImports re past imports out = foldl (\mre path -> do{
        re <- mre;
        compile re (path:past) path out
        }) (return re) imports
        
writeCode :: String -> E.ErrLineMonad String -> IO ()
writeCode f code = case code of
        Left (sp, ep, err) -> if sp == ep then 
                        putStrLn $ "Error on line " ++ show sp ++ ":" ++ show err
                else putStrLn $ "Error between lines " ++ show sp ++ 
                        "and " ++ show ep ++  ":" ++ show err
        Right res -> do
                writeFile f res
                putStrLn $ "Success! File written to " ++ f
                