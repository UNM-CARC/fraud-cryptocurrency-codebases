module Parser where

import qualified Text.Parsec.Token as Tok
import           Text.ParserCombinators.Parsec
import           Text.Parsec.Language (haskellStyle)


lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where operations = ["//", "/*", "*", "*/"]
        names      = []
        style      = haskellStyle {Tok.reservedOpNames = operations,
                                   Tok.reservedNames   = names}

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

parseTree :: String -> Either ParseError Term
parseTree = parse (conts term) "<stdin>"
