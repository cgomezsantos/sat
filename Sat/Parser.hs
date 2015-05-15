{-# Language OverloadedStrings #-}
-- | Parser for formulas given a signature.
module Sat.Parser (parseSignatureFormula, symbolList, getErrString) where

import Sat.Core

import qualified Data.Set as S
import qualified Data.Text as T(unpack,Text,concat)
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr(OperatorTable,Operator(..),Assoc(..),buildExpressionParser)
import Text.Parsec.Error
import Data.List(nub,intercalate)
import Control.Monad.Identity
import Control.Applicative ((<$>),(<$),(<*>),(<*),(*>))

type ParserF a b = ParsecT String a Identity b

-- Tabla para los operadores lógicos.
type ParserTable a = OperatorTable String a Identity Formula

-- 〈 ∀x:True:Tr(A) 〉
quantInit :: T.Text
quantInit = "〈"
quantEnd :: T.Text
quantEnd = "〉"
quantSep :: T.Text
quantSep = ":"
quantInitTrad :: T.Text
quantInitTrad = "("
quantEndTrad :: T.Text
quantEndTrad = ")"
quantSepTrad :: T.Text
quantSepTrad = "."

forallSymbol :: T.Text
forallSymbol = "∀"
existsSymbol :: T.Text
existsSymbol = "∃"
andSymbol :: T.Text
andSymbol = "∧"
orSymbol :: T.Text
orSymbol = "∨"
implSymbol :: T.Text
implSymbol = "⇒"
negSymbol :: T.Text
negSymbol = "¬"
equivSymbol :: T.Text
equivSymbol = "≡"
eqSymbol :: T.Text
eqSymbol = "="

forAllExpresion :: T.Text
forAllExpresion = T.concat [ quantInit
                           , forallSymbol," "
                           , quantSep, " "
                           , quantSep, " "
                           , quantEnd
                           ]

existsExpresion :: T.Text
existsExpresion = T.concat [ quantInit
                           , existsSymbol, " "
                           , quantSep, " "
                           , quantSep, " "
                           , quantEnd, " "
                           ]

-- | List of logical symbols, used to allow the insertion through a
-- menu.
symbolList :: [T.Text]
symbolList = [ forAllExpresion
             , existsExpresion
             , andSymbol
             , orSymbol
             , implSymbol
             , negSymbol
             , equivSymbol
             , eqSymbol
             ]
              
              

quantRepr :: [String]
quantRepr = map T.unpack [forallSymbol,existsSymbol]

folConRepr :: [String]
folConRepr = ["True","False"]

folOperators :: [String]
folOperators = map T.unpack [andSymbol,orSymbol,implSymbol,negSymbol,equivSymbol]

table :: Signature -> ParserTable a
table sig = [ [Prefix $ reservedOp (lexer sig) (T.unpack negSymbol) >> return Neg]
           ,  [Infix (reservedOp (lexer sig) (T.unpack andSymbol) >> return And) AssocNone
              ,Infix (reservedOp (lexer sig) (T.unpack orSymbol) >> return Or) AssocNone]
           ,  [Infix (reservedOp (lexer sig) (T.unpack equivSymbol) >> return Equiv) AssocNone]
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
                     }
lexer :: Signature -> GenTokenParser String u Identity
lexer sig = (lexer' sig) { whiteSpace = oneOf " \t" >> return ()}

spaces' :: ParserF s ()
spaces' = optional (many space)
                     
parseFormula :: Signature -> ParserF s Formula
parseFormula sig = buildExpressionParser (table sig) (parseSubFormula sig)

parseSubFormula :: Signature -> ParserF s Formula
parseSubFormula sig =
     (parseRelation sig <?> "relación")
    <|> parseTrue sig
    <|> parseFalse sig
    <|> parseEq sig
    <|> parsePredicate sig   
    <|> parseForAll sig
    <|> parseExists sig
    <|> parseQuantTrad sig forallSymbol ForAll
    <|> parseQuantTrad sig existsSymbol Exist
    <|> parens (lexer sig) (parseFormula sig)

parseTrue,parseFalse,parseEq,parseForAll,parseExists :: Signature -> ParserF s Formula
parseTrue sig = FTrue <$ reserved (lexer sig) "True"
parseFalse sig = FFalse <$ reserved (lexer sig) "False"

parseEq sig = Eq <$> parseTerm sig <* spaces' <* string (T.unpack eqSymbol) <* spaces' <*> (parseTerm sig <?> "término")

parseTerm :: Signature -> ParserF s Term
parseTerm sig = (Con <$> (parseConst sig)
             <|> Var <$> parseVariable sig)
             <?> "constante o variable"
--            <|> parseFunc sig
           
parseVariable :: Signature -> ParserF s Variable
parseVariable sig =  try $ 
                    lexeme (lexer sig) ((:) <$> lower <*> many alphaNum) >>= 
                    \v -> return $ Variable v

parseConst :: Signature -> ParserF s Constant
parseConst sig = S.foldr ((<|>) . pConst) (fail "Constante") (constants sig)
    where pConst c = c <$ (reserved (lexer sig) . conName) c

-- Asumimos la aridad de las funciones es mayor o igual a 1.
-- parseFunc :: Signature -> ParserF s Term
-- parseFunc sig = S.foldr ((<|>) . pFunc) (fail "Función") (functions sig)
--     where pFunc f = (reserved lexersig . fname) f >>
--                     symbol lexersig "." >>
--                     sepBy (parseTerm sig) (symbol lexersig ".") >>= \subterms ->
--                     if length subterms /= farity f
--                        then fail "Aridad de la función"
--                        else return (Fun f subterms)
--           lexersig = lexer sig



parseForAll sig = parseQuant (T.unpack forallSymbol) sig >>= \(v,r,t) -> return (ForAll v (Impl r t))
parseExists sig = parseQuant (T.unpack existsSymbol) sig >>= \(v,r,t) -> return (Exist v (And r t))

parseQuantTrad :: Signature  -> T.Text -> (Variable -> Formula -> a) -> ParsecT String u Identity a
parseQuantTrad sig sym con = try $ con
                   <$ symbol (lexer sig) (T.unpack quantInitTrad)                   
                   <* symbol (lexer sig) (T.unpack sym)
                   <*> (parseVariable sig <?> "variable")
                   <* symbol (lexer sig) (T.unpack quantSepTrad) 
                   <*> parseFormula sig 
                   <* symbol (lexer sig) (T.unpack quantEndTrad)

parseQuant :: String -> Signature -> ParserF s (Variable,Formula,Formula)
parseQuant sym sig = try $
                symbol (lexer sig) (T.unpack quantInit) >> spaces' >>
                symbol (lexer sig) sym >> 
                ((spaces' >> parseVariable sig) <?> "variable") >>= 
                \v -> symbol (lexer sig) (T.unpack quantSep)  >> parseRange sig >>=
                \r -> symbol (lexer sig) (T.unpack quantSep)  >> (spaces' >> parseFormula sig) >>=
                \t -> symbol (lexer sig) (T.unpack quantEnd) >> return (v,r,t)

parseRange :: Signature -> ParserF s Formula
parseRange sig = try (spaces' *> parseFormula sig) <|> (FTrue <$ spaces') 

-- Asumimos la aridad de los predicados es mayor o igual a 1.
parsePredicate :: Signature -> ParserF s Formula
parsePredicate sig = S.foldr ((<|>) . pPred) (fail "Predicado") (predicates sig)
    where pPred p = Pred p <$ (reserved lexersig . pname) p 
                           <* symbol lexersig "."
                           <*> parseTerm sig
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
parseSignatureFormula signature = parse (parseFormula signature <* eof) ""


getErrString :: ParseError -> String
getErrString = ("Fórmula mal formada" ++) . showErrorMessages' "ó" 
                                 "Error desconocido" 
                                 "Se espera: " 
                                 "Inesperado: " 
                                 . errorMessages

showErrorMessages' ::
    String -> String -> String -> String -> [Message] -> String
showErrorMessages' msgOr msgUnknown msgExpecting msgUnExpected msgs
    | null msgs = msgUnknown
    | otherwise = concat $ map (". "++) $ clean $
                 [showSysUnExpect,showUnExpect,showExpect]
    where
      (sysUnExpect,msgs1) = span ((SysUnExpect "") ==) msgs
      (unExpect,msgs2)    = span ((UnExpect    "") ==) msgs1
      (expect,_)          = span ((Expect      "") ==) msgs2

      showExpect      = showMany msgExpecting expect
      showUnExpect    = showMany msgUnExpected unExpect
      showSysUnExpect | not (null unExpect) ||
                        null sysUnExpect = ""
                      | null firstMsg    = msgUnExpected
                      | otherwise        = msgUnExpected ++ " " ++ firstMsg
          where
              firstMsg  = messageString (head sysUnExpect)


      -- helpers
      showMany pre msg = case clean (map messageString msg) of
                            [] -> ""
                            ms | null pre  -> commasOr ms
                               | otherwise -> pre ++ " " ++ commasOr ms

      commasOr []       = ""
      commasOr [m]      = m
      commasOr ms       = interspComma (init ms) ++ " " ++ msgOr ++ " " ++ last ms

      interspComma      = intercalate ", " . clean

      clean             = nub . filter (not . null)
