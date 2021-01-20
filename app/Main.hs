module Main where

import Parser
import Control.Monad
import System.Console.Haskeline

main :: IO ()
main = forever $ do
  putStr "Scheme> "
  line <- getLine
  print $ parseProgram line
