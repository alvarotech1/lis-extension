module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST

-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
                  whiteSpace lis
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , reservedNames = ["true","false","skip","if",
                                                     "then","else","end",
                                                     "while","do","return"]
                                  , reservedOpNames = [  "+"
                                                       , "-"
                                                       , "*"
                                                       , "/"
                                                       , "<"
                                                       , ">"
                                                       , "&"
                                                       , "++"
                                                       , "--"
                                                       , "|"
                                                       , "="
                                                       , ";"
                                                       , "~"
                                                       , ":="
                                                       , "%"
                                                       ]
                                   }
                                 )
----------------------------------
--- Parser de expressiones enteras
-----------------------------------
{--
chainl p op x
parsea 0 o más ocurrencias de p separadas por op
Retorna el valor que se obtiene al aplicar todas las
funciones retornadas por op a los valores retornados
por p

t := t + t | m
term = chainl factor (do {symbol "+"; return (+)})
factor = integer <|> parens term
--}
intexp :: Parser IntExp
intexp  = chainl1 term addopp

term = chainl1 factor multopp

factor = try (parens lis intexp)
         <|> try (do reservedOp lis "-"
                     f <- factor
                     return (UMinus f))
         <|> try (do n <- integer lis
                     return (Const (fromInteger n)))
         <|> try (do str <- identifier lis
                     return (Var str))


multopp = do try (reservedOp lis "*")
             return Times
          <|> do try (reservedOp lis "/")
                 return Div
          <|> do try (reservedOp lis "%")
                 return Mod

addopp = do try (reservedOp lis "+")
            return Plus
         <|> do try (reservedOp lis "-")
                return Minus

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------
boolexp :: Parser BoolExp
boolexp  = chainl1 boolexp2 (try (do reservedOp lis "|"
                                     return Or))

boolexp2 = chainl1 boolexp3 (try (do reservedOp lis "&"
                                     return And))

boolexp3 = try (parens lis boolexp)
           <|> try (do reservedOp lis "~"
                       b <- boolexp3
                       return (Not b))
           <|> intcomp
           <|> boolvalue

intcomp = try (do i <- intexp
                  c <- compopp
                  j <- intexp
                  return (c i j))

compopp = try (do reservedOp lis "="
                  return Eq)
          <|> try (do reservedOp lis "<"
                      return Lt)
          <|> try (do reservedOp lis ">"
                      return Gt)

boolvalue = try (do reserved lis "true"
                    return BTrue)
            <|> try (do reserved lis "false"
                        return BFalse)

listaIdent :: Parser [Variable]
listaIdent = parens lis (identifier lis `sepBy` comma lis)

listaArgs :: Parser [IntExp]
listaArgs = parens lis (intexp `sepBy` comma lis)

-----------------------------------
--- Parser de comandos
-----------------------------------

-- se cambio el parser para que se pida ; al final de cada linea.
comm :: Parser Comm
comm = do
  cs <- endBy1 comm2 (reservedOp lis ";")
  return (foldr1 Seq cs)


comm2 = try (do reserved lis "skip"
                return Skip)
        <|> try (do reserved lis "if"
                    cond <- boolexp
                    reserved lis "then"
                    case1 <- comm
                    reserved lis "else"
                    case2 <- comm
                    reserved lis "end"
                    return (Cond cond case1 case2))
        <|> try (do var <- identifier lis
                    reservedOp lis ":="
                    reserved lis "call"
                    fname <- identifier lis
                    args <- listaArgs
                    return (LetCall var fname args))
        <|> try (do str <- identifier lis
                    reservedOp lis ":="
                    e <- intexp
                    return (Let str e))
        <|> try (do reserved lis "while"
                    cond <- boolexp
                    reserved lis "do"
                    body <- comm
                    reserved lis "end"
                    return (While cond body))
        <|> try (do var <- identifier lis
                    reservedOp lis "++"
                    return (Inc var))
        <|> try (do var <- identifier lis
                    reservedOp lis "--"
                    return (Dec var))
        <|> try (do reserved lis "sub"
                    fname <- identifier lis
                    params <- listaIdent
                    reserved lis "do"
                    cuerpo <- comm
                    reserved lis "end"
                    return (Sub fname params cuerpo))
        <|> try (do reserved lis "call"
                    fname <- identifier lis
                    args <- listaArgs
                    return (Call fname args))
        <|> try (do reserved lis "return"
                    e <- intexp
                    return (Return e))
            



------------------------------------
-- Funcion de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
