
{-# LANGUAGE TemplateHaskell #-}
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
  | TkOpenBrace | TkCloseBrace | TkOpenBraceBar | TkCloseBraceBar
  | TkBar | TkEscBar | TkAmp | TkPlus | TkMinus | TkBang | TkTilde
  | TkStar | TkEquals | TkComma | TkDot | TkSemicolon | TkPercent | TkSlash
  | TkHat | TkLT | TkGT | TkLE | TkGE | TkBangEq | TkQuestionMark
  | TkDoubleQuestionMark | TkDoubleLt | TkDoubleGt | TkDoubleEq
  | TkDoublePlus | TkDoubleDash | TkBitChoose  | TkDoubleBar | TkDoubleAmp
  | TkDoubleColon | TkColon | TkPlusEquals
  | TkIf | TkElse | TkBit | TkInt | TkVoid | TkImplements | TkReturn | TkReorder
  | TkAssert
  | TkRepeat | TkWhile | TkFor | TkGenerator | TkHarness | TkTrue | TkFalse | TkDo
  | TkPragma | TkOptions | TkRef | TkStruct | TkNew | TkNull
  | TkID String
  | TkString String
  | TkInteger Int
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
    ('{', TkOpenBrace),
    ('}', TkCloseBrace),
    ('|', TkBar),
    ('*', TkStar),
    ('!', TkBang),
    ('~', TkTilde),
    ('&', TkAmp),
    ('+', TkPlus),
    ('%', TkPercent),
    ('/', TkSlash),
    ('-', TkMinus),
    ('^', TkHat),
    ('<', TkLT),
    ('>', TkGT),
    ('=', TkEquals),
    (',', TkComma),
    ('.', TkDot),
    (';', TkSemicolon),
    ('?', TkQuestionMark),
    (':', TkColon)
  ]

doubles :: [(String, Token)]
doubles = [
    ("??", TkDoubleQuestionMark),
    (">=", TkGE),
    ("<=", TkLE),
    (">>", TkDoubleGt),
    ("<<", TkDoubleLt),
    ("==", TkDoubleEq),
    ("!=", TkBangEq),
    ("++", TkDoublePlus),
    ("--", TkDoubleDash),
    ("+=", TkPlusEquals),
    ("||", TkDoubleBar),
    ("\\|", TkEscBar),
    ("&&", TkDoubleAmp),
    ("::", TkDoubleColon),
    ("{|", TkOpenBraceBar),
    ("|}", TkCloseBraceBar)
  ]

triples :: [(String, Token)]
triples = [
    ("{|}", TkBitChoose)
  ]

keywords :: [(String, Token)]
keywords = [
    ("if", TkIf),
    ("else", TkElse),
    ("bit", TkBit),
    ("int", TkInt),
    ("void", TkVoid),
    ("implements", TkImplements),
    ("return", TkReturn),
    ("reorder", TkReorder),
    ("assert", TkAssert),
    ("generator", TkGenerator),
    ("harness", TkHarness),
    ("ref", TkRef),
    ("struct", TkStruct),
    ("new", TkNew),
    ("null", TkNull),
    ("true", TkTrue),
    ("false", TkFalse),
    ("repeat", TkRepeat),
    ("for", TkFor),
    ("do", TkDo),
    ("while", TkWhile),
    ("pragma", TkPragma),
    ("options", TkOptions)
  ]

isIDChar :: Char -> Bool
isIDChar c = isAlphaNum c || c == '_'

-- Return True if the character could be the first character of an ID
isIDChar0 :: Char -> Bool
isIDChar0 c = isAlpha c || c == '_'

closeblockcomment :: String -> String
closeblockcomment ('*':'/':cs) = cs
closeblockcomment [] = []
closeblockcomment (c:cs) = closeblockcomment cs

lex :: ParserMonad Token
lex = do
    text <- get
    case text of
      [] -> return TkEOF
      ('/':'*':cs) -> put (closeblockcomment cs) >> lex
      ('/':'/':cs) -> put (dropWhile (/= '\n') cs) >> lex
      ('#':' ':cs) -> put (dropWhile (/= '\n') cs) >> lex
      (a:b:c:cs) | Just tok <- lookup [a, b, c] triples -> put cs >> return tok
      (a:b:cs) | Just tok <- lookup [a, b] doubles -> put cs >> return tok
      (c:cs) | Just tok <- lookup c singles -> put cs >> return tok
      (c:cs) | isSpace c -> put cs >> lex
      (c:cs) | isIDChar0 c ->
         let (ns, rest) = span isIDChar cs
         in case (c:ns) of
              id | Just t <- lookup id keywords -> put rest >> return t
                 | otherwise -> put rest >> return (TkID $ id)
      (c:cs) | isDigit c ->
         let (ns, rest) = span isDigit cs
         in put rest >> return (TkInteger . read $ c:ns)
      ('"':cs) | (ns, '"':rest) <- break (== '"') cs ->
         put rest >> return (TkString ns)
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

