module Dusty where

import DependentLambda

data Statement
        = Assign String (Maybe Expr) Expr
        | Native String Expr 
        | ADT String [(String,Expr)] --Type [(Constructor, ConsType)]
        | Inline String --a section of target language code inlined in the source file
        | Comment String
        deriving(Show)
        
type Dusty = [Statement]