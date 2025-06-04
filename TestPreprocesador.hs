module Main where

import System.Environment (getArgs)
import Preprocesador (preprocesarGoto)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [archivo] -> do
      contenido <- readFile archivo
      case preprocesarGoto contenido of
        Left err -> do
          putStrLn "❌ Error en el preprocesador:"
          putStrLn err
        Right resultado -> do
          putStrLn "✅ Código luego del preprocesador:\n"
          putStrLn resultado
    _ -> putStrLn "Uso: runghc TestPreprocesador.hs archivo.lis"
