module AST where

-- Identificadores de Variable
type Variable = String

-- Expresiones Aritmeticas
data IntExp = Const Int
            | Var Variable
            | UMinus IntExp
            | Plus IntExp IntExp
            | Minus IntExp IntExp
            | Times IntExp IntExp
            | Div IntExp IntExp
            | Mod IntExp IntExp
 deriving Show

-- Expresiones Booleanas
data BoolExp = BTrue
             | BFalse
             | Eq IntExp IntExp
             | Lt IntExp IntExp
             | Gt IntExp IntExp
             | And BoolExp BoolExp
             | Or BoolExp BoolExp
             | Not BoolExp
 deriving Show

-- Comandos (sentencias)
data Comm = Skip
          | Let Variable IntExp
          | LetCall Variable String [IntExp]
          | Seq Comm Comm
          | Cond BoolExp Comm Comm
          | While BoolExp Comm
          | Inc Variable
          | Dec Variable
          | Sub String [Variable] Comm       
          | Call String [IntExp]
          | Return IntExp          
 deriving Show
