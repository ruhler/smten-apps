
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Lexer (
    Token(..), ParserMonad, lexer, failE,
    ) where

import Smten.Prelude

import Smten.Control.Monad.Error
import Smten.Control.Monad.State

import Smten.Data.Char (isSpace, isAlphaNum, isAlpha, isDigit)
import Smten.Data.Functor ((<$>))

data Token =
    TkOpenParen | TkCloseParen | TkOpenBracket | TkCloseBracket
  | TkOpenBrace | TkCloseBrace
  | TkBar | TkAmp | TkPlus | TkBang | TkStar | TkEquals | TkComma | TkSemicolon
  | TkHat
  | TkDoubleQuestionMark | TkDoubleLt | TkDoubleGt
  | TkIf | TkElse | TkBit | TkInt | TkImplements | TkReturn | TkAssert
  | TkID String
  | TkInteger Int
  | TkEOF

instance Show Token where
    show TkOpenParen = "TkOpenParen"
    show TkCloseParen = "TkCloseParen"
    show TkOpenBracket = "TkOpenBracket"
    show TkCloseBracket = "TkCloseBracket"
    show TkOpenBrace = "TkOpenBrace"
    show TkCloseBrace = "TkCloseBrace"
    show TkBar = "TkBar"
    show TkStar = "TkStar"
    show TkBang = "TkBang"
    show TkAmp = "TkAmp"
    show TkPlus = "TkPlus"
    show TkHat = "TkHat"
    show TkEquals = "TkEquals"
    show TkComma = "TkComma"
    show TkSemicolon = "TkSemicolon"
    show TkDoubleQuestionMark = "TkDoubleQuestionMark"
    show TkDoubleLt = "TkDoubleLt"
    show TkDoubleGt = "TkDoubleGt"
    show TkIf = "TkIf"
    show TkElse = "TkElse"
    show TkBit = "TkBit"
    show TkInt = "TkInt"
    show TkImplements = "TkImplements"
    show TkReturn = "TkReturn"
    show TkAssert = "TkAssert"
    show (TkID x) = "TkID " ++  show x
    show (TkInteger x) = "TkInteger " ++ show x
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
    ('*', TkStar),
    ('!', TkBang),
    ('&', TkAmp),
    ('+', TkPlus),
    ('^', TkHat),
    ('=', TkEquals),
    (',', TkComma),
    (';', TkSemicolon)
  ]

doubles :: [(String, Token)]
doubles = [
    ("??", TkDoubleQuestionMark),
    (">>", TkDoubleGt),
    ("<<", TkDoubleLt)
  ]

keywords :: [(String, Token)]
keywords = [
    ("if", TkIf),
    ("else", TkElse),
    ("bit", TkBit),
    ("int", TkInt),
    ("implements", TkImplements),
    ("return", TkReturn),
    ("assert", TkAssert)
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
         in put rest >> return (TkInteger . read $ c:ns)
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

