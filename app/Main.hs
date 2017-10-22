module Main where

import Scheme
import Repl
import System.Environment
import Control.Monad.Except

main :: IO ()
main = do
        args <- getArgs
        case length args of
          0 -> Repl.runRepl
          1 -> Repl.evalAndPrint $ args !! 0
          otherwise -> putStrLn "Program takes 0 or 1 argument"
