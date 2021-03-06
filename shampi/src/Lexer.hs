
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fprof-auto-top #-}
module Lexer (
    Token(..), ParserMonad, lexer, failE,
    ) where

import Smten.Prelude

import Smten.Control.Monad.Error
import Smten.Control.Monad.State

import Smten.Data.Char (isSpace, isAlphaNum, isAlpha, isDigit, chr, digitToInt)

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

lexer :: String -> [Token]
lexer text =
  case text of
    [] -> []
    (a:b:cs) | Just tok <- lookup [a, b] doubles -> tok : lexer cs
    (c:cs) | Just tok <- lookup c singles ->  tok : lexer cs
    (c:cs) | isSpace c -> lexer cs
    (c:cs) | isAlpha c -> 
       case span isIDChar cs of
         (ns, rest) | Just t <- lookup (c:ns) keywords -> t : lexer rest
                    | otherwise -> TkID (c:ns) : lexer rest
    (c:cs) | isDigit c ->
       case span isDigit cs of
          (ns, rest) -> (TkInt (read (c:ns))) : lexer rest
    ('\'':c:'\'':cs) -> TkChar c : lexer cs
    ('\\':a:b:c:cs) | isDigit a && isDigit b && isDigit c -> 
       case mkseq a b c of
          !cv -> TkChar cv : lexer cs
    ('"':cs) | (ns, '"':rest) <- break (== '"') cs ->
       TkString ns : lexer rest
    ('/':'*':cs) -> lexer (closeblockcomment cs)
    ('/':'/':cs) -> lexer (dropWhile (/= '\n') cs)
    cs -> error $ "fail to lex: " ++ cs

