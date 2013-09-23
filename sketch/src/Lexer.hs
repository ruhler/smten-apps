
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Lexer (
    Token(..), ParserMonad, lexer, failE, read_int,
    ) where

import Smten.Prelude

import Smten.Control.Monad.Error
import Smten.Control.Monad.State

import Smten.Data.Char (isSpace, isAlphaNum, isAlpha, isDigit, chr, digitToInt)
import Smten.Data.Functor ((<$>))

data Token =
    TkOpenParen | TkCloseParen | TkOpenBracket | TkCloseBracket
  | TkOpenBrace | TkCloseBrace
  | TkBar | TkAmp | TkEquals | TkComma | TkSemicolon | TkDoubleQuestionMark
  | TkBit | TkImplements | TkReturn
  | TkID String
  | TkInt Int
  | TkEOF

instance Show Token where
    show TkOpenParen = "TkOpenParen"
    show TkCloseParen = "TkCloseParen"
    show TkOpenBracket = "TkOpenBracket"
    show TkCloseBracket = "TkCloseBracket"
    show TkOpenBrace = "TkOpenBrace"
    show TkCloseBrace = "TkCloseBrace"
    show TkBar = "TkBar"
    show TkAmp = "TkAmp"
    show TkEquals = "TkEquals"
    show TkComma = "TkComma"
    show TkSemicolon = "TkSemicolon"
    show TkDoubleQuestionMark = "TkDoubleQuestionMark"
    show TkBit = "TkBit"
    show TkImplements = "TkImplements"
    show TkReturn = "TkReturn"
    show (TkID x) = "TkID " ++  show x
    show (TkInt x) = "TkInt " ++ show x
    show TkEOF = "TkEOF"

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
    ('{', TkOpenBrace),
    ('}', TkCloseBrace),
    ('|', TkBar),
    ('&', TkAmp),
    ('=', TkEquals),
    (',', TkComma),
    (';', TkSemicolon)
  ]

doubles :: [(String, Token)]
doubles = [
    ("??", TkDoubleQuestionMark)
  ]

keywords :: [(String, Token)]
keywords = [
    ("bit", TkBit),
    ("implements", TkImplements),
    ("return", TkReturn)
  ]

isIDChar :: Char -> Bool
isIDChar c = isAlphaNum c || c == '_'

closeblockcomment :: String -> String
closeblockcomment ('*':'/':cs) = cs
closeblockcomment [] = []
closeblockcomment (c:cs) = closeblockcomment cs

lex :: ParserMonad Token
lex = do
    text <- get
    case text of
      [] -> return TkEOF
      (a:b:cs) | Just tok <- lookup [a, b] doubles -> put cs >> return tok
      (c:cs) | Just tok <- lookup c singles -> put cs >> return tok
      (c:cs) | isSpace c -> put cs >> lex
      (c:cs) | isAlpha c ->
         let (ns, rest) = span isIDChar cs
         in case (c:ns) of
              id | Just t <- lookup id keywords -> put rest >> return t
                 | otherwise -> put rest >> return (TkID $ id)
      (c:cs) | isDigit c ->
         let (ns, rest) = span isDigit cs
         in put rest >> return (TkInt . read_int $ c:ns)
      ('/':'*':cs) -> put (closeblockcomment cs) >> lex
      ('/':'/':cs) -> put (dropWhile (/= '\n') cs) >> lex
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

read_int' :: Int -> String -> Int
read_int' x ('0':xs) = read_int' (x*10 + 0) xs
read_int' x ('1':xs) = read_int' (x*10 + 1) xs
read_int' x ('2':xs) = read_int' (x*10 + 2) xs
read_int' x ('3':xs) = read_int' (x*10 + 3) xs
read_int' x ('4':xs) = read_int' (x*10 + 4) xs
read_int' x ('5':xs) = read_int' (x*10 + 5) xs
read_int' x ('6':xs) = read_int' (x*10 + 6) xs
read_int' x ('7':xs) = read_int' (x*10 + 7) xs
read_int' x ('8':xs) = read_int' (x*10 + 8) xs
read_int' x ('9':xs) = read_int' (x*10 + 9) xs
read_int' x _ = x

read_int :: String -> Int
read_int ('-':xs) = negate (read_int xs)
read_int xs = read_int' 0 xs

