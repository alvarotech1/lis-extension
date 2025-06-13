module Main where

import System.Environment (getArgs)
import Parser (parseComm)
import Preprocesador (preprocesarGoto)

-- Modificar este import para usar diferentes evaluadores
import Eval3
---------------------------------------------------------

main :: IO ()
main = do arg:_ <- getArgs
          run arg

-- Ejecuta un programa a partir de su archivo fuente
run :: [Char] -> IO ()
run file = do
  s <- readFile file
  case preprocesarGoto s of
    Left err     -> putStrLn ("Preprocesador error: " ++ err)
    Right limpio -> case parseComm file limpio of
                      Left error -> print error
                      Right t    -> print (eval t)
                      --Right t    -> print t        --imprimir sin evaluar (para testear Parser)
