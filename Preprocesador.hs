module Preprocesador  where

import Data.Char (isDigit, isSpace)
import Text.Read (readMaybe)

type LineaNumerada = (Int, String)


esUntil :: String -> Bool
esUntil s = trim s == "until;" 

extraerBloque :: [LineaNumerada] -> Int -> Either String ([String], [Int])
extraerBloque prog dest =
  case dropWhile ((/= dest) . fst) prog of
    [] -> Left $ "Línea de destino " ++ show dest ++ " no encontrada"
    ((dNum,dCod):rest) ->
      let (bloque, resto) = break(esUntil . snd) rest
      in case resto of
           [] -> Left $ "Falta 'until' para la línea " ++ show dest
           ((nUntil,_):_) ->
             let codigos  = dCod : map snd bloque
                 numerosE = dNum : map fst bloque ++ [nUntil]
             in Right (codigos, numerosE)


procesarLineas :: [LineaNumerada] -> [String]
procesarLineas prog = resolverGoto prog []
  where
    resolverGoto :: [LineaNumerada] -> [Int] -> [String]
    resolverGoto [] _ = []
    resolverGoto ((n,c):xs) eliminados
      | n `elem` eliminados = resolverGoto xs eliminados      -- ya fue copiado
      | esUntil c          = resolverGoto xs eliminados      -- nunca llega acá salvo error
      | esGoto c =
          case extraerBloque prog (extraerDestino c) of
            Left err               -> error err
            Right (cop, numsDel)   -> cop ++ resolverGoto xs (numsDel ++ eliminados)
      | otherwise           = c : resolverGoto xs eliminados


preprocesarGoto :: String -> Either String String
preprocesarGoto contenido =
  let lineas = lines contenido
      lineasNumeradas = map parsearLinea lineas
  in case sequence lineasNumeradas of
       Nothing    -> Left "Hay líneas mal formadas. Asegurate de usar el formato: <nro>: <codigo>"
       Just lista -> Right (unlines (procesarLineas lista))

parsearLinea :: String -> Maybe LineaNumerada
parsearLinea str =
  case break (== ':') str of
    (numStr, ':' : resto) ->
      case readMaybe (trim numStr) of
        Just n  -> Just (n, trim resto)
        Nothing -> Nothing
    _ -> Nothing

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

esGoto :: String -> Bool
esGoto s = take 4 s == "goto"           

extraerDestino :: String -> Int
extraerDestino str =
  let resto  = drop 4 str
      limpio = takeWhile isDigit (dropWhile isSpace resto)
  in case readMaybe limpio of
       Just n  -> n
       Nothing -> error "goto mal formado"
