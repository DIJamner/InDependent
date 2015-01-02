import DependentLambda
import DependentLambdaParser
import InDependentParser
import InDependent
import InDependentToJS
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
        
        "compile" -> do
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
                
writeCode :: String -> E.ErrLineMonad String -> IO ()
writeCode f code = case code of
        Left (sp, ep, err) -> if sp == ep then 
                        putStrLn $ "Error on line " ++ show sp ++ ":" ++ show err
                else putStrLn $ "Error between lines " ++ show sp ++ 
                        "and " ++ show ep ++  ":" ++ show err
        Right res -> do
                writeFile f res
                putStrLn $ "Success! File written to " ++ f
                