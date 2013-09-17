
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
    ','     { TkComma }
    ';'     { TkSemicolon }
    '??'    { TkDoubleQuestionMark }
    'bit'   { TkBit }
    'implements'   { TkImplements }
    'return'   { TkReturn }
    id      { TkID $$ }
    int     { TkInt $$ }

%%

sketch :: { SKProg }
 : decl         { [$1] }
 | sketch decl   { $2 : $1 }

decl :: { SKDecl }
 : type id '(' args ')' '{' stmts '}'
    { SKFun $2 $1 $4 $7 Nothing }
 | type id '(' args ')' 'implements' id '{' stmts '}'
    { SKFun $2 $1 $4 $9 (Just $7) }

type :: { SKType }
 : 'bit' { SKBit }
 | 'bit' '[' int ']' { SKBitVector $3 }

args :: { [(SKType, Name)] }
 : arg { [$1] }
 | args ',' arg { $1 ++ [$3] }

arg :: { (SKType, Name) }
 : type id { ($1, $2) }

stmts :: { [SKStmt] }
 : stmt     { [$1] }
 | stmts stmt { $1  ++ [$2] }

stmt :: { SKStmt }
 : 'return' expr ';' { SKReturn $2 }

expr :: { SKExpr }
 : '(' expr ')'     { $2 }
 | expr '&' expr    { SKBVAnd $1 $3 }
 | expr '|' expr    { SKBVOr $1 $3 }
 | '??' { SKHole 0 }
 | int { SKInt $1 }
 | id { SKVar $1 }
 | expr '[' expr ']' { SKAccess $1 $3 }


{

parseError :: Token -> ParserMonad a
parseError tok = do
    x <- get
    failE $ "parser error at " ++ show tok ++ "\n when parsing: " ++ x

seq :: a -> b -> b
seq = const id

}

