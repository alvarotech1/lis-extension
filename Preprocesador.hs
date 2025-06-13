module Preprocesador (preprocesarGoto) where

import Data.Char (isDigit, isSpace)
import Text.Read (readMaybe)

type LineaNumerada = (Int, String)

preprocesarGoto :: String -> Either String String
preprocesarGoto contenido =
  let lineas = lines contenido
      lineasNumeradas = map parsearLinea lineas
  in case sequence lineasNumeradas of
       Nothing   -> Left "Hay líneas mal formadas. Asegurate de usar el formato: <nro>: <codigo>"
       Just lista -> Right (unlines (procesarLineas lista))

-- parsea la línea de "10: x := 1" a (10, "x := 1")
parsearLinea :: String -> Maybe LineaNumerada
parsearLinea str =
  case break (== ':') str of
    (numStr, ':' : resto) ->
      case readMaybe (trim numStr) of
        Just n  -> Just (n, trim resto)
        Nothing -> Nothing
    _ -> Nothing

-- quita espacios al principio y al final
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

procesarLineas :: [LineaNumerada] -> [String]
procesarLineas lista =
  let destinos = [ (n, extraerDestino c) | (n, c) <- lista, esGoto c ]
      lineasFiltradas = filter (\(n, _) -> not (n `elem` (map snd destinos))) lista
  in map (reemplazarGoto lista destinos) lineasFiltradas


esGoto :: String -> Bool
esGoto str = take 4 str == "goto"


extraerDestino :: String -> Int
extraerDestino str =
  let resto = drop 4 str
      limpio = takeWhile isDigit (dropWhile isSpace resto)
  in case readMaybe limpio of
       Just n  -> n
       Nothing -> error "goto mal formado"


reemplazarGoto :: [LineaNumerada] -> [(Int, Int)] -> LineaNumerada -> String
reemplazarGoto lista destinos (n, c)
  | esGoto c =
      case lookup n destinos of
        Just d  -> case lookup d lista of
                     Just nuevoCodigo -> nuevoCodigo
                     Nothing -> error ("Línea de destino " ++ show d ++ " no encontrada")
        Nothing -> error ("No se encontró destino para goto en línea " ++ show n)
  | otherwise = c
