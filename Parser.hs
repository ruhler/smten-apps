
import Prelude hiding (lex)

import Control.Monad.Error
import Control.Monad.State

import Data.Char (isSpace, isAlphaNum, isAlpha, isDigit)
import Data.Functor ((<$>))

data Token =
    TkOpenParen | TkCloseParen
  | TkBar | TkComma | TkSemicolon | TkColon | TkColonEquals
  | TkVar | TkCfg | TkReg | TkFix | TkAssert | TkIn
  | TkID String
  | TkInt Integer
  | TkString String
  | TkEOF
    deriving (Show, Eq)

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
    ('|', TkBar),
    (';', TkSemicolon),
    (',', TkComma),
    (':', TkColon)
  ]

doubles :: [(String, Token)]
doubles = [
    (":=", TkColonEquals)
  ]

keywords :: [(String, Token)]
keywords = [
    ("var", TkVar),
    ("cfg", TkCfg),
    ("reg", TkReg),
    ("fix", TkFix),
    ("assert", TkAssert),
    ("in", TkIn)
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
         in put rest >> return (TkInt . read $ c:ns)
      ('"':cs) | (ns, '"':rest) <- break (== '"') cs ->
         put rest >> return (TkString ns)
      ('/':'*':cs) -> put (closeblockcomment cs) >> lex
      cs -> failE $ "fail to lex: " ++ cs

-- Get all the remaining tokens.
tokens :: ParserMonad [Token]
tokens = do
    t <- lex
    case t of
        TkEOF -> return []
        _ -> (:) t <$> tokens

main :: IO ()
main = do
    input <- getContents
    let toks = runStateT tokens input
    putStrLn $ show toks
        
