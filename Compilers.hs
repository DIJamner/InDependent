
import qualified Errors as E
import System.Environment

--type FlagProcessor = Map String (DataFlow -> DataFlow)

data DataFlow = DataFlow Input Process Output

defaultDF :: DataFlow
defaultDF = DataFlow defaultIn compile defaultOut

type Input = String -> IO String --input may or may not perform IO. This does not include getArgs.

defaultIn :: Input
defaultIn = return

type Output = ([E.Error], String) -> IO ()

defaultOut :: Output
defaultOut (es, s) = (putStrLn (show es ++ s))

type Process = String -> ([E.Error], String)--compile is a process


--processors :: FlagProcessor
--processors = ...

--process :: FlagProcessor -> [String] -> DataFlow
--process p [] =

compile :: Process
compile s = ([], "Let's pretend it worked.")

executeDF :: String -> DataFlow -> IO ()
executeDF s (DataFlow i p o) = o $ (i s) >>= p

main :: IO ()
main = executeDF "" defaultDF