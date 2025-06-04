-- Preprocesador.hs - Versión más simple, para estudiante
module Preprocesador (preprocesarGoto) where

import Data.Char (isDigit, isSpace)
import Text.Read (readMaybe)

-- Tipo simple: cada línea tiene un número y un texto
-- Ej: (10, "x := 1")
type LineaNumerada = (Int, String)

-- Funcion principal
preprocesarGoto :: String -> Either String String
preprocesarGoto contenido =
  let lineas = lines contenido
      -- Intentamos parsear todas las líneas
      lineasNumeradas = map parsearLinea lineas
  in
    if any esNada lineasNumeradas then
      Left "Hay líneas mal formadas. Asegurate de usar el formato: <nro>: <codigo>"
    else
      let lista = map sacarJusto lineasNumeradas
          resultado = procesarLineas lista
      in Right (unlines resultado)

-- Parsear una línea tipo "10: x := 1" a (10, "x := 1")
parsearLinea :: String -> Maybe LineaNumerada
parsearLinea str =
  case break (== ':') str of
    (numStr, ':' : resto) ->
      case readMaybe (trim numStr) of
        Just n  -> Just (n, trim resto)
        Nothing -> Nothing
    _ -> Nothing

-- Ver si un Maybe es Nothing
esNada :: Maybe a -> Bool
esNada Nothing = True
esNada _       = False

-- Extraer valor de un Just
sacarJusto :: Maybe a -> a
sacarJusto (Just x) = x
sacarJusto Nothing  = error "No debería pasar"

-- Quita espacios al principio y al final
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Procesar todas las líneas:
-- 1. Detectar goto
-- 2. Reemplazar por el destino
-- 3. Eliminar la línea destino
procesarLineas :: [LineaNumerada] -> [String]
procesarLineas lista =
  let destinos = [ (n, extraerDestino c) | (n, c) <- lista, esGoto c ]
      lineasFiltradas = filter (\(n, _) -> not (n `elem` (map snd destinos))) lista
      lineasFinales = map (reemplazarGoto lista destinos) lineasFiltradas
  in lineasFinales

-- Ver si una línea es un goto
esGoto :: String -> Bool
esGoto str = take 4 str == "goto"

-- Extraer el número de destino de un goto
extraerDestino :: String -> Int
extraerDestino str =
  let resto = drop 4 str
      limpio = takeWhile isDigit (dropWhile isSpace resto)
  in case readMaybe limpio of
       Just n  -> n
       Nothing -> error "goto mal formado"

-- Reemplaza una línea si es un goto
reemplazarGoto :: [LineaNumerada] -> [(Int, Int)] -> LineaNumerada -> String
reemplazarGoto lista destinos (n, c)
  | esGoto c =
      let destino = lookup n destinos
      in case destino of
           Just d  -> case lookup d lista of
                         Just nuevoCodigo -> nuevoCodigo
                         Nothing -> error ("Línea de destino " ++ show d ++ " no encontrada")
           Nothing -> error ("No se encontró destino para goto en línea " ++ show n)
  | otherwise = c
