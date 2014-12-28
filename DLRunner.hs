import DependentLambda
import DependentLambdaParser
import DustyParser
import Dusty
import DustyToJS
import qualified Errors as E
import qualified JSWriter as JS

import System.Environment
import Text.Parsec

main :: IO ()
main = do
        args <- getArgs
        case args !! 0 of
                "parse" -> do
                        putStrLn $ "Parsing " ++ (args !! 1)
                        print $ parse expr "lambdapi" $ args !! 1
                "normalize" -> print $ (normalize [] []) `fmap` (parse expr "lambdapi" $ args !! 1)
                "infertype" -> print $ (inferType [] []) `fmap` (parse expr "lambdapi" $ args !! 1)
                "dusty" -> processDusty 1 args print
                
processDusty :: Int -> [String] -> (E.ErrLineMonad String -> IO ()) -> IO ()
processDusty i args out = case args !! i of
        "-i" -> do
                src <- readFile (args !! (i+1))
                processDusty (i+2) (args ++ [src]) out
        "-o" -> processDusty (i+2) args (writeCode (args !! (i+1)))
        "validate" -> do
                let code = case parse dusty "dusty" (args !! (i + 1)) of
                        Left err -> Left (sourceLine $ errorPos err, E.ParsecError err)
                        Right res -> return res
                let valid = (validate []) =<< code
                putStrLn "Validate: "
                print code
                putStrLn "\nResult: "
                print valid
        "print" -> print $ parse dusty "dusty" (args !! (i + 1))
        
        "compile" -> do
                let dcode = case parse dusty "dusty" (args !! (i + 1)) of
                        Left err -> Left (sourceLine $ errorPos err, E.ParsecError err)
                        Right res -> return res
                let valid = (validate []) =<< dcode
                case valid of
                        Left err -> print err
                        Right re -> do
                                let jscode = (dustyToJS re) `fmap` dcode
                                out ((JS.toText 0) `fmap` jscode)
                
writeCode :: String -> E.ErrLineMonad String -> IO ()
writeCode f code = case code of
        Left (l,err) -> putStrLn $ "Error on line " ++ show l ++ ":" ++ show err
        Right res -> do
                writeFile f res
                putStrLn $ "Success! File written to " ++ f
                