{-# Language OverloadedStrings #-}
module Sat.Parser where

import Sat.Core
import Sat.Signatures.Figures

import qualified Data.Set as S
import Data.Text(unpack)
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr(OperatorTable,Operator(..),Assoc(..),buildExpressionParser)

import Control.Monad.Identity
import Control.Applicative ((<$>),(<$),(<*>))

type ParserF a b = ParsecT String a Identity b

-- Tabla para los operadores lógicos.
type ParserTable a = OperatorTable String a Identity Formula


-- 〈∀x:True:Tr(A)〉
quantInit = "〈"
quantEnd = "〉"
quantSep = ":"

forallSymbol = "∀"
existsSymbol = "∃"
andSymbol = "∧"
orSymbol = "∨"
implSymbol = "⇒"
negSymbol = "¬"
equivSymbol = "≡"


quantRepr :: [String]
quantRepr = [forallSymbol,existsSymbol]

folConRepr :: [String]
folConRepr = ["True","False"]

folOperators :: [String]
folOperators = map unpack [andSymbol,orSymbol,implSymbol,negSymbol,equivSymbol]

table :: Signature -> ParserTable a
table sig = [ [Prefix $ reservedOp (lexer sig) (unpack negSymbol) >> return Neg]
           ,  [Infix (reservedOp (lexer sig) (unpack andSymbol) >> return And) AssocLeft
              ,Infix (reservedOp (lexer sig) (unpack orSymbol) >> return Or) AssocLeft]
           ,  [Infix (reservedOp (lexer sig) (unpack equivSymbol) >> return Equiv) AssocLeft]
           ,  [Infix (reservedOp (lexer sig) (unpack implSymbol) >> return Impl) AssocLeft]
           ]
                            
             

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
                     


                     
parseFormula :: Signature -> ParserF s Formula
parseFormula sig = buildExpressionParser (table sig) (parseSubFormula sig)
               <?> "Parser error: Fórmula mal formada"


parseSubFormula :: Signature -> ParserF s Formula
parseSubFormula sig =
        parseTrue sig
    <|> parseFalse sig
    <|> parseForAll sig
    <|> parseExists sig
    <|> parsePredicate sig
    <|> parseRelation sig
    <?> "subfórmula"

parseTrue sig = reserved (lexer sig) "True" >> return FTrue
parseFalse sig = reserved (lexer sig) "False" >> return FFalse

parseForAll sig = parseQuant forallSymbol sig >>= \(v,r,t) -> return (ForAll v (Impl r t))
parseExists sig = parseQuant existsSymbol sig >>= \(v,r,t) -> return (Exist v (Impl r t))

parseQuant :: String -> Signature -> ParserF s (Variable,Formula,Formula)
parseQuant sym sig = try $ 
                symbol (lexer sig) quantInit >>
                symbol (lexer sig) sym >>
                (parseVariable sig <?> "Cuantificador sin variable") >>= 
                \v -> symbol (lexer sig) quantSep  >> parseFormula sig >>=
                \r -> symbol (lexer sig) quantSep  >> parseFormula sig >>=
                \t -> symbol (lexer sig) quantEnd >> return (v,r,t)
                
parsePredicate :: Signature -> ParserF s Formula
parsePredicate sig = S.foldr ((<|>) . pPred) (fail "Predicado") (predicates sig)
    where pPred p = (reserved lexersig . pname) p >>
                    parens lexersig (sepBy (parseTerm sig) (symbol lexersig ",")) >>= \subterms ->
                    if length subterms /= 1
                       then fail "Los predicados deben tener un solo argumento"
                       else return (Pred p $ head subterms)
          lexersig = lexer sig

parseRelation :: Signature -> ParserF s Formula
parseRelation sig = S.foldr ((<|>) . pRel) (fail "Relación") (relations sig)
    where pRel r = (reserved lexersig . rname) r >>
                    parens lexersig (sepBy (parseTerm sig) (symbol lexersig ",")) >>= \subterms ->
                    if length subterms /= rarity r
                       then fail "Aridad de la relación"
                       else return (Rel r subterms)
          lexersig = lexer sig

parseSignatureFormula :: Signature -> String -> Either ParseError Formula
parseSignatureFormula signature = parse (parseFormula signature) ""
          
parseFiguresTerm :: String -> Either ParseError Term 
parseFiguresTerm = parse (parseTerm figuras)  "TEST"

parseFiguresFormula :: String -> Either ParseError Formula
parseFiguresFormula = parseSignatureFormula figuras
