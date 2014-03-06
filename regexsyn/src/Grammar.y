-- vim: ft=haskell
{

module Grammar (parseRegEx) where

import Smten.Prelude
import Smten.Control.Monad.State
import qualified Smten.Data.Map as Map

import RegEx
import Lexer

}

%name parseRegEx regex
%tokentype { Token }
%error { parseError }
%monad { ParserMonad }
%lexer { lexer } { TkEOF }

%token
    '('     { TkOpenParen }
    ')'     { TkCloseParen }
    '['     { TkOpenBracket }
    ']'     { TkCloseBracket }
    '|'     { TkBar }
    '-'     { TkDash }
    '*'     { TkAsterisk }
    '+'     { TkPlus }
    '?'     { TkQuestionMark }
    char  { TkChar $$ }

%%

regex :: { RegEx }
 : branch               { $1 }
 | branch '|' regex     { orR $1 $3 }

branch :: { RegEx }
 : piece                { $1 }
 | piece branch         { concatR $1 $2 }

piece :: { RegEx }
 : atom '*'             { starR $1 }
 | atom '+'             { plusR $1 }
 | atom '?'             { optionR $1 }
 | atom                 { $1 }

atom :: { RegEx }
 : '(' regex ')'            { $2 }
 | '(' ')'                  { epsilonR }
 | '[' char '-' char ']'    { rangeR $2 $4 }
 | char                     { atomR $1 }


{

parseError :: Token -> ParserMonad a
parseError tok = do
    x <- get
    failE $ "parser error at " ++ show tok ++ "\n when parsing: " ++ x

seq :: a -> b -> b
seq = const id

}

