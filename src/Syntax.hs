module Syntax where

data Expr = Num Int
          | Boolean Bool
          | Var String Expr
          | Func String [String] Expr
          | Call String 
          | Cond Expr Expr Expr
          | Quote Expr
          | Lambda [String] Expr
          | Application Expr [Expr]
          deriving (Eq, Ord, Show)
