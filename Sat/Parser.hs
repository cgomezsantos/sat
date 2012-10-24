{-# Language OverloadedStrings #-}
module Sat.Parser where

import Sat.Core
import Sat.Signatures.Figures

import qualified Data.Set as S
import Data.Text(unpack)
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

import Control.Monad.Identity
import Control.Applicative ((<$>),(<$),(<*>))

type ParserF a b = ParsecT String a Identity b

quantInit = "〈"

quantEnd = "〉"

quantRepr :: [String]
quantRepr = [unpack "∀", unpack "∃"]

folConRepr :: [String]
folConRepr = ["True","False"]

folOperators :: [String]
folOperators = map unpack ["∧","∨","⇒","¬","≡"]

rNames :: Signature -> [String]
rNames sig =  [quantInit,quantEnd]
         ++ S.toList (S.map conName $ constants sig)
         ++ S.toList (S.map fname $ functions sig)
         ++ S.toList (S.map rname $ relations sig)
         ++ S.toList (S.map pname $ predicates sig)
         ++ quantRepr ++ folConRepr

-- Para lexical analisys.
lexer' :: Signature -> TokenParser u
lexer' sig = makeTokenParser $
            emptyDef { reservedOpNames = folOperators
                     , reservedNames = rNames sig
                     , identStart  = letter
                     , identLetter = alphaNum <|> char '_'
                     --, opLetter = newline
                     }

lexer sig = (lexer' sig) { whiteSpace = oneOf " \t" >> return ()}

parseTerm :: Signature -> ParserF s Term
parseTerm sig = Con <$> (parseConst sig)
            <|> parseFunc sig
            <|> Var <$> parseVariable sig
           
parseVariable :: Signature -> ParserF s Variable
parseVariable sig =  try $ 
                    lexeme (lexer sig) ((:) <$> lower <*> many alphaNum) >>= 
                    \v -> return $ Variable v

parseConst :: Signature -> ParserF s Constant
parseConst sig = S.foldr ((<|>) . pConst) (fail "Constante") (constants sig)
    where pConst c = c <$ (reserved (lexer sig) . conName) c
                     

parseFunc :: Signature -> ParserF s Term
parseFunc sig = S.foldr ((<|>) . pFunc) (fail "Función") (functions sig)
    where pFunc f = (reserved lexersig . fname) f >>
                    parens lexersig (sepBy (parseTerm sig) (symbol lexersig ",")) >>= \subterms ->
                    if length subterms /= farity f
                       then fail "Aridad de la función"
                       else return (Fun f subterms)
          lexersig = lexer sig
                     

parseFiguresTerm :: String -> Either ParseError Term 
parseFiguresTerm = parse (parseTerm figuras)  "TEST"
                     
-- parseFormula :: ParserF s Formula
-- parseFormula = buildExprParser operatorTable (subexpr (peParenFlag $ getExprState st))
--                <?> "Parser error: Expresi&#243;n mal formada"
