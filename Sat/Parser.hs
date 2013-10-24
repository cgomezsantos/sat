{-# Language OverloadedStrings #-}
-- | Parser for formulas given a signature.
module Sat.Parser (parseSignatureFormula, symbolList, getErrString) where

import Sat.Core
import Sat.Signatures.Figures

import qualified Data.Set as S
import qualified Data.Text as T(unpack,Text(..),concat)
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr(OperatorTable,Operator(..),Assoc(..),buildExpressionParser)
import Text.Parsec.Error
import Data.List(nub)
import Control.Monad.Identity
import Control.Applicative ((<$>),(<$),(<*>),(<*))

type ParserF a b = ParsecT String a Identity b

-- Tabla para los operadores lógicos.
type ParserTable a = OperatorTable String a Identity Formula

-- 〈∀x:True:Tr(A)〉
quantInit = "〈"
quantEnd = "〉"
quantSep = ":"
quantInitTrad = "("
quantEndTrad = ")"
quantSepTrad = "."


forallSymbol = "∀"
existsSymbol = "∃"
andSymbol = "∧"
orSymbol = "∨"
implSymbol = "⇒"
negSymbol = "¬"
equivSymbol = "≡"
eqSymbol = "="

forAllExpresion = T.concat [ quantInit
                           , forallSymbol," "
                           , quantSep, " "
                           , quantSep, " "
                           , quantEnd
                           ]
existsExpresion = T.concat [ quantInit
                           , existsSymbol, " "
                           , quantSep, " "
                           , quantSep, " "
                           , quantEnd, " "
                           , eqSymbol
                           ]

-- | List of logical symbols, used to allow the insertion through a
-- menu.
symbolList = [ forAllExpresion
             , existsExpresion
             , andSymbol
             , orSymbol
             , implSymbol
             , negSymbol
             , equivSymbol
             ]
              
              

quantRepr :: [String]
quantRepr = map T.unpack [forallSymbol,existsSymbol]

folConRepr :: [String]
folConRepr = ["True","False"]

folOperators :: [String]
folOperators = map T.unpack [andSymbol,orSymbol,implSymbol,negSymbol,equivSymbol]

table :: Signature -> ParserTable a
table sig = [ [Prefix $ reservedOp (lexer sig) (T.unpack negSymbol) >> return Neg]
           ,  [Infix (reservedOp (lexer sig) (T.unpack andSymbol) >> return And) AssocLeft
              ,Infix (reservedOp (lexer sig) (T.unpack orSymbol) >> return Or) AssocLeft]
           ,  [Infix (reservedOp (lexer sig) (T.unpack equivSymbol) >> return Equiv) AssocLeft]
           ,  [Infix (reservedOp (lexer sig) (T.unpack implSymbol) >> return Impl) AssocLeft]
           ]
                            
             

rNames :: Signature -> [String]
rNames sig =  (map T.unpack [quantInit,quantEnd])
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

-- Asumimos la aridad de las funciones es mayor o igual a 1.
parseFunc :: Signature -> ParserF s Term
parseFunc sig = S.foldr ((<|>) . pFunc) (fail "Función") (functions sig)
    where pFunc f = (reserved lexersig . fname) f >>
                    symbol lexersig "." >>
                    sepBy (parseTerm sig) (symbol lexersig ".") >>= \subterms ->
                    if length subterms /= farity f
                       then fail "Aridad de la función"
                       else return (Fun f subterms)
          lexersig = lexer sig
                     
parseFormula :: Signature -> ParserF s Formula
parseFormula sig = buildExpressionParser (table sig) (parseSubFormula sig)
--               <?> "Fórmula mal formada"

parseSubFormula :: Signature -> ParserF s Formula
parseSubFormula sig =
        parseTrue sig
    <|> parseFalse sig
    <|> parseEq sig
    <|> parseForAll sig
    <|> parseExists sig
    <|> parseQuantTrad sig forallSymbol ForAll
    <|> parseQuantTrad sig existsSymbol Exist
    <|> parsePredicate sig 
    <|> parseRelation sig
    <?> "subfórmula"

parseTrue sig = reserved (lexer sig) "True" >> return FTrue
parseFalse sig = reserved (lexer sig) "False" >> return FFalse

parseEq sig = Eq <$> parseTerm sig <* string "=" <*> parseTerm sig

parseForAll sig = parseQuant (T.unpack forallSymbol) sig >>= \(v,r,t) -> return (ForAll v (Impl r t))
parseExists sig = parseQuant (T.unpack existsSymbol) sig >>= \(v,r,t) -> return (Exist v (And r t))

parseQuantTrad sig sym con = try $ con
                   <$ symbol (lexer sig) (T.unpack quantInitTrad)
                   <* symbol (lexer sig) (T.unpack sym)
                   <*> (parseVariable sig <?> "Cuantificador sin variable")
                   <* symbol (lexer sig) (T.unpack quantSepTrad) 
                   <*> parseFormula sig 
                   <* symbol (lexer sig) (T.unpack quantEndTrad)

parseQuant :: String -> Signature -> ParserF s (Variable,Formula,Formula)
parseQuant sym sig = try $ 
                symbol (lexer sig) (T.unpack quantInit) >>
                symbol (lexer sig) sym >>
                (parseVariable sig <?> "Cuantificador sin variable") >>= 
                \v -> symbol (lexer sig) (T.unpack quantSep)  >> parseFormula sig >>=
                \r -> symbol (lexer sig) (T.unpack quantSep)  >> parseFormula sig >>=
                \t -> symbol (lexer sig) (T.unpack quantEnd) >> return (v,r,t)

-- Asumimos la aridad de los predicados es mayor o igual a 1.
parsePredicate :: Signature -> ParserF s Formula
parsePredicate sig = S.foldr ((<|>) . pPred) (fail "Predicado") (predicates sig)
    where pPred p = (reserved lexersig . pname) p >>
                    symbol lexersig "." >>
                    sepBy (parseTerm sig) (symbol lexersig ".") >>= \subterms ->
                    if length subterms /= 1
                       then fail "Los predicados deben tener un solo argumento"
                       else return (Pred p $ head subterms)
          lexersig = lexer sig

-- Asumimos la aridad de las relaciones es mayor o igual a 1.
parseRelation :: Signature -> ParserF s Formula
parseRelation sig = S.foldr ((<|>) . pRel) (fail "Relación") (relations sig)
    where pRel r = (reserved lexersig . rname) r >>
                    symbol lexersig "." >>
                    sepBy (parseTerm sig) (symbol lexersig ".") >>= \subterms ->
                    if length subterms /= rarity r
                       then fail "Aridad de la relación"
                       else return (Rel r subterms)
          lexersig = lexer sig

-- | Given a signature tries to parse a string as a well-formed formula.
parseSignatureFormula :: Signature -> String -> Either ParseError Formula
parseSignatureFormula signature = parse (parseFormula signature >>= 
                                         \f -> eof >> return f) ""
          
parseFiguresTerm :: String -> Either ParseError Term 
parseFiguresTerm = parse (parseTerm figuras)  "TEST"

parseFiguresFormula :: String -> Either ParseError Formula
parseFiguresFormula = parseSignatureFormula figuras


getErrString :: ParseError -> String
getErrString = ("Fórmula mal formada" ++) . showErrorMessages' "ó" 
                                 "Error desconocido" 
                                 "Se espera: " 
                                 "Inesperado: " 
                                 "Debería haber algo más" . errorMessages

showErrorMessages' ::
    String -> String -> String -> String -> String -> [Message] -> String
showErrorMessages' msgOr msgUnknown msgExpecting msgUnExpected msgEndOfInput msgs
    | null msgs = msgUnknown
    | otherwise = concat $ map (". "++) $ clean $
                 [showSysUnExpect,showUnExpect,showExpect]
    where
      (sysUnExpect,msgs1) = span ((SysUnExpect "") ==) msgs
      (unExpect,msgs2)    = span ((UnExpect    "") ==) msgs1
      (expect,messages)   = span ((Expect      "") ==) msgs2

      showExpect      = showMany msgExpecting expect
      showUnExpect    = showMany msgUnExpected unExpect
      showSysUnExpect | not (null unExpect) ||
                        null sysUnExpect = ""
                      | null firstMsg    = msgUnExpected ++ " " ++ msgEndOfInput
                      | otherwise        = msgUnExpected ++ " " ++ firstMsg
          where
              firstMsg  = messageString (head sysUnExpect)

      showMessages      = showMany "" messages

      -- helpers
      showMany pre msgs = case clean (map messageString msgs) of
                            [] -> ""
                            ms | null pre  -> commasOr ms
                               | otherwise -> pre ++ " " ++ commasOr ms

      commasOr []       = ""
      commasOr [m]      = m
      commasOr ms       = commaSep (init ms) ++ " " ++ msgOr ++ " " ++ last ms

      commaSep          = seperate ", " . clean

      seperate   _ []     = ""
      seperate   _ [m]    = m
      seperate sep (m:ms) = m ++ sep ++ seperate sep ms

      clean             = nub . filter (not . null)
