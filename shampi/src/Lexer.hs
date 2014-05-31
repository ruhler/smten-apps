
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fprof-auto-top #-}
module Lexer (
    Token(..), ParserMonad, lexer, failE,
    ) where

import Smten.Prelude

import Smten.Control.Monad.Error
import Smten.Control.Monad.State

import Smten.Data.Char (isSpace, isAlphaNum, isAlpha, isDigit, chr, digitToInt)
import Smten.Data.Functor ((<$>))

data Token =
    TkOpenParen | TkCloseParen | TkOpenBracket | TkCloseBracket
  | TkBar | TkComma | TkSemicolon | TkColon | TkColonEquals | TkDoubleDot
  | TkDash | TkAsterisk | TkPlus | TkQuestionMark
  | TkVal | TkVar | TkCfg | TkReg | TkFix | TkConcat | TkStar | TkOr | TkNot
  | TkAssert | TkIn | TkContains | TkEquals
  | TkID String
  | TkInt Int
  | TkString String
  | TkChar Char
  | TkEOF
    deriving (Show)

-- | State is the text remaining to be parsed.
--   Left is parse failed with error message
--   Right is sucessful result of the parse.
type ParserMonad = StateT String (Either String)

-- Parse failure
failE :: String -> ParserMonad a
failE = throwError

-- List of single character tokens.
singles :: [(Char, Token)]
singles = [
    ('(', TkOpenParen),
    (')', TkCloseParen),
    ('[', TkOpenBracket),
    (']', TkCloseBracket),
    ('|', TkBar),
    ('-', TkDash),
    ('*', TkAsterisk),
    ('+', TkPlus),
    ('?', TkQuestionMark),
    (';', TkSemicolon),
    (',', TkComma),
    (':', TkColon)
  ]

doubles :: [(String, Token)]
doubles = [
    ("..", TkDoubleDot),
    (":=", TkColonEquals)
  ]

keywords :: [(String, Token)]
keywords = [
    ("val", TkVal),
    ("var", TkVar),
    ("cfg", TkCfg),
    ("reg", TkReg),
    ("fix", TkFix),
    ("assert", TkAssert),
    ("in", TkIn),
    ("contains", TkContains),
    ("equals", TkEquals),
    ("concat", TkConcat),
    ("star", TkStar),
    ("or", TkOr),
    ("not", TkNot)
  ]

isIDChar :: Char -> Bool
isIDChar c = isAlphaNum c || c == '_'

closeblockcomment :: String -> String
closeblockcomment ('*':'/':cs) = cs
closeblockcomment [] = []
closeblockcomment (c:cs) = closeblockcomment cs

mkseq :: Char -> Char -> Char -> Char
mkseq a b c = chr $ (100 * digitToInt a) + (10 * digitToInt b) + digitToInt c

lex :: ParserMonad Token
lex = do
    text <- get
    case text of
      [] -> return TkEOF
      (a:b:cs) | Just tok <- lookup [a, b] doubles ->  {-# SCC "DOUBLE" #-} put cs >> return tok
      (c:cs) | Just tok <- lookup c singles ->  {-# SCC "SINGLE" #-} put cs >> return tok
      (c:cs) | isSpace c -> {-# SCC "SPACE" #-} put cs >> lex
      (c:cs) | isAlpha c -> {-# SCC "ALPHA" #-}
         let (ns, rest) = span isIDChar cs
         in case (c:ns) of
              id | Just t <- lookup id keywords -> put rest >> return t
                 | otherwise -> put rest >> return (TkID $ id)
      (c:cs) | isDigit c -> {-# SCC "DIGIT" #-}
         case span isDigit cs of
            (ns, rest) -> put rest >> return (TkInt (read (c:ns)))
      ('\'':c:'\'':cs) -> {-# SCC "CHAR" #-} put cs >> return (TkChar c)
      ('\\':a:b:c:cs) | isDigit a && isDigit b && isDigit c -> {-# SCC "CHARSEQ" #-}
         case mkseq a b c of
            !cv -> put cs >> return (TkChar cv)
      ('"':cs) | (ns, '"':rest) <- {-# SCC "STR" #-} break (== '"') cs ->
         put rest >> return (TkString ns)
      ('/':'*':cs) -> {-# SCC "BLOCKCOMMENT"#-} put (closeblockcomment cs) >> lex
      ('/':'/':cs) -> {-# SCC "LINECOMMENT"#-} put (dropWhile (/= '\n') cs) >> lex
      cs -> failE $ "fail to lex: " ++ cs

-- Get all the remaining tokens.
tokens :: ParserMonad [Token]
tokens = do
    t <- lex
    case t of
        TkEOF -> return []
        _ -> (:) t <$> tokens

lexer :: (Token -> ParserMonad a) -> ParserMonad a
lexer output = lex >>= output

