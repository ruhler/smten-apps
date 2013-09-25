
-- vim: ft=haskell
{

{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Grammar (parseSketch) where

import Smten.Prelude
import Smten.Control.Monad.State
import qualified Smten.Data.Map as Map

import Lexer
import Sketch

}

%name parseSketch sketch
%tokentype { Token }
%error { parseError }
%monad { ParserMonad }
%lexer { lexer } { TkEOF }

%token
    '('     { TkOpenParen }
    ')'     { TkCloseParen }
    '['     { TkOpenBracket }
    ']'     { TkCloseBracket }
    '{'     { TkOpenBrace }
    '}'     { TkCloseBrace }
    '|'     { TkBar }
    '&'     { TkAmp }
    '!'     { TkBang }
    '*'     { TkStar }
    '='     { TkEquals }
    ','     { TkComma }
    ';'     { TkSemicolon }
    '??'    { TkDoubleQuestionMark }
    'if'    { TkIf }
    'bit'   { TkBit }
    'implements'   { TkImplements }
    'return'   { TkReturn }
    id      { TkID $$ }
    int     { TkInt $$ }

%%

sketch :: { Prog }
 : decl         { [$1] }
 | sketch decl   { $2 : $1 }

decl :: { Decl }
 : type id '(' args ')' '{' stmts '}'
    { FunD $2 $1 $4 $7 Nothing }
 | type id '(' args ')' 'implements' id '{' stmts '}'
    { FunD $2 $1 $4 $9 (Just $7) }

type :: { Type }
 : 'bit' { BitT }
 | 'bit' '[' int ']' { BitsT $3 }

args :: { [(Type, Name)] }
 : arg { [$1] }
 | args ',' arg { $1 ++ [$3] }

arg :: { (Type, Name) }
 : type id { ($1, $2) }

stmts :: { [Stmt] }
 : stmt     { [$1] }
 | stmts stmt { $1  ++ [$2] }

stmt :: { Stmt }
 : 'return' expr ';' { ReturnS $2 }
 | type id '=' expr ';' { BlockS [DeclS $1 $2, UpdateS $2 $4] }
 | type id ';' { DeclS $1 $2 }
 | id '=' expr ';' { UpdateS $1 $3 }
 | id '[' expr ']' '=' expr ';' { ArrUpdateS $1 $3 $6 }
 | '{' stmts '}' { BlockS $2 }
 | 'if' '(' expr ')' stmt { IfS $3 $5 }

expr :: { Expr }
 : '(' expr ')'     { $2 }
 | expr '&' expr    { AndE $1 $3 }
 | expr '|' expr    { OrE $1 $3 }
 | '!' expr         { NotE $2 }
 | '??' { HoleE UnknownT }
 | '{' '*' '}' { HoleE UnknownT }   -- TODO: is {*} really the same as ??
 | int { IntE $1 }
 | id { VarE $1 }
 | expr '[' expr ']' { AccessE $1 $3 }


{

parseError :: Token -> ParserMonad a
parseError tok = do
    x <- get
    failE $ "parser error at " ++ show tok ++ "\n when parsing: " ++ x

seq :: a -> b -> b
seq = const id

}

