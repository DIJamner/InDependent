module ImportParser where

import DependentLambdaParser
import DependentLambda
import InDependent
import InDependentParser
import qualified Errors as E

import Text.Parsec
import Text.Parsec.String (Parser)
--TODO: use language lib in parsec

inde :: Parser ([String], LinedInDependent)
inde = do
        imps <- try $ many importFile
        body <- many1 (do
                s <- statement
                char '\n'
                whitespace
                return s)
        return (imps, body)

importFile :: Parser String --TODO: file names in error data
importFile = do
        string "import"
        spaces
        f <- filename
        spaces
        char '\n'
        return f
                
filename :: Parser String
filename = do
        base <- many1 alphaNum
        ext <- optionMaybe (do
                char '.'
                n <- many1 alphaNum
                return n)
        let ret = case ext of
                Nothing -> base
                Just n -> base ++ "/" ++ n
        return ret
        