module Syntax ( Var, Lambda, Exp ) where

type Var = String

data Lambda = Var :=> Exp

data Exp = Ref Var
         | Lam Lambda
         | Exp :@ Exp
