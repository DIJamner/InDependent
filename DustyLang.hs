module DustyLang where
import TypedLambda
import Assignment

type DustyLang = [DStatement]

data DStatement 
        = DAssign (Assignment TypedLambda)