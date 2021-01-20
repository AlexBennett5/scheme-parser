module Parser where

import Data.Char
import Control.Monad
import Control.Applicative
import Syntax

newtype Parser a = Parser { parse :: String -> [(a, String)] }

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return a = Parser (\cs -> [(a, cs)])
  p >>= f  = Parser (\cs -> concat [parse (f a) cx | (a, cx) <- parse p cs])

instance MonadPlus Parser where
  mzero = Parser (\cs -> [])
  mplus p q = Parser (\cs -> parse p cs ++ parse q cs)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p `mplus` q) cs of
                            []      -> []
                            (x:xs)  -> [x])

instance Alternative Parser where
  empty = mzero
  (<|>) = (+++)
 
item  :: Parser Char
item  = Parser (\cs -> case cs of
                        ""      -> []
                        (c:cs)  -> [(c, cs)])

sat     :: (Char -> Bool) -> Parser Char
sat pred = do c <- item
              if pred c then return c else mzero

char  :: Char -> Parser Char
char c = sat (c ==)

digit :: Parser Char
digit = sat isDigit

string       :: String -> Parser String
string ""     = return ""
string (c:cs) = do char c
                   string cs
                   return (c:cs)

alpha :: Parser Char
alpha = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum 

manyp   :: Parser a -> Parser [a]
manyp p  = manyp1 p <|> return []

manyp1  :: Parser a -> Parser [a]
manyp1 p = do a <- p
              as <- manyp p
              return (a:as)

space :: Parser String
space  = manyp (sat isSpace)

token  :: Parser a -> Parser a
token p = do space
             a <- p
             space
             return a

symb   :: String -> Parser String
symb cs = token (string cs)

parens :: Parser a -> Parser a
parens p = do symb "("
              a <- p
              symb ")"
              return a

number :: Parser Int
number = do s   <- string "-" <|> return []
            cs  <- manyp1 digit
            return $ read (s ++ cs)
           
-- ==========================

parseProgram :: String -> [Expr]
parseProgram code = runParser program code

runParser :: Parser a -> String -> a
runParser p s = case parse p s of
                  [(res, [])] -> res
                  [(_, rest)] -> error ("Parser did not consume entire stream: " ++ rest)
                  _           -> error "Parser error."

program :: Parser [Expr]
program = manyp form 

form :: Parser Expr
form = defn <|> expr

defn :: Parser Expr
defn = defnVar <|> defnFunc

defnVar :: Parser Expr
defnVar = parens $ do symb "define"
                      v  <- variable
                      ex <- expr
                      return $ Var v ex 

variable :: Parser String
variable = token identifier <|> token primitive

defnFunc :: Parser Expr
defnFunc = parens $ do symb "define"
                       symb "("
                       id   <- variable
                       args <- manyp variable
                       symb ")"
                       ex   <- expr
                       return $ Func id args ex
                   
identifier :: Parser String
identifier = do c   <- alpha
                cs  <- manyp alphanum
                return (c:cs)

primitive :: Parser String
primitive = string "+"
   <|> string "-"
   <|> string "*"
   <|> string "/"
   <|> string "<"
   <|> string ">"
   <|> string "car"
   <|> string "cdr"

expr :: Parser Expr
expr = constant
   <|> call
   <|> quote
   <|> lambda
   <|> cond
   <|> application
   <|> parens expr

constant :: Parser Expr
constant = boolean <|> num <|> parens boolean <|> parens num

boolean :: Parser Expr
boolean = boolTrue <|> boolFalse

boolTrue :: Parser Expr
boolTrue = do b <- symb "#t"
              return $ Boolean True

boolFalse :: Parser Expr
boolFalse = do b <- symb "#f"
               return $ Boolean False

integer :: Parser Int
integer = token number

num :: Parser Expr
num = Num <$> integer
 
call :: Parser Expr
call = Call <$> variable

quote :: Parser Expr
quote = do symb "'"
           ex <- expr
           return $ Quote ex

lambda :: Parser Expr
lambda = parens $ do symb "lambda"
                     f <- parens $ manyp variable
                     b <- expr
                     return $ Lambda f b

cond :: Parser Expr
cond = parens $ do symb "if"
                   i <- expr
                   t <- expr
                   e <- expr
                   return $ Cond i t e

application :: Parser Expr
application = parens $ do e  <- expr
                          es <- manyp1 expr
                          return $ Application e es

