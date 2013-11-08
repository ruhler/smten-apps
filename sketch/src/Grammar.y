
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
    '<'     { TkLT }
    '>'     { TkGT }
    '<='    { TkLE }
    '>='    { TkGE }
    '+'     { TkPlus }
    '-'     { TkMinus }
    '!'     { TkBang }
    '~'     { TkTilde }
    '^'     { TkHat }
    '*'     { TkStar }
    '%'     { TkPercent }
    '/'     { TkSlash }
    '='     { TkEquals }
    ','     { TkComma }
    ';'     { TkSemicolon }
    '||'    { TkDoubleBar }
    '&&'    { TkDoubleAmp }
    '::'    { TkDoubleColon }
    ':'     { TkColon }
    '=='    { TkDoubleEq }
    '!='    { TkBangEq }
    '>>'    { TkDoubleGt }
    '<<'    { TkDoubleLt }
    '++'    { TkDoublePlus }
    '--'    { TkDoubleDash }
    '?'     { TkQuestionMark }
    '??'    { TkDoubleQuestionMark }
    '{|}'    { TkBitChoose }
    'if'    { TkIf }
    'repeat'    { TkRepeat }
    'while'    { TkWhile }
    'for'    { TkFor }
    'else'  { TkElse }
    'bit'   { TkBit }
    'int'   { TkInt }
    'implements'   { TkImplements }
    'return'   { TkReturn }
    'assert'   { TkAssert }
    'generator'{ TkGenerator }
    'true'  { TkTrue }
    'false' { TkFalse }
    id      { TkID $$ }
    integer { TkInteger $$ }

%left '||'
%left '&&'
%left '|'
%left '^'
%left '&'
%left '==' '!='
%left '<' '>' '<=' '>='
%left '<<' '>>'
%left '+' '-'
%left '*' '%' '/'
%right '!' '~'

%%

sketch :: { Prog }
 : decl         { [$1] }
 | sketch decl   { $2 : $1 }

decl :: { Decl }
 : type id '(' args ')' '{' stmts '}'
    { FunD $2 (Function (FunT $1 (map fst $4)) (map snd $4) (blockS $7)) NormalF }
 | type id '(' args ')' 'implements' id '{' stmts '}'
    { FunD $2 (Function (FunT $1 (map fst $4)) (map snd $4) (blockS $9)) (WithSpecF $7) }
 | 'generator' type id '(' args ')' '{' stmts '}'
    { FunD $3 (Function (FunT $2 (map fst $5)) (map snd $5) (blockS $8)) GeneratorF }
 | type id '=' expr ';' { VarD $1 $2 $4 }

type :: { Type }
 : 'bit' { BitT }
 | type '[' expr ']' { ArrT $1 $3 }
 | 'int' { IntT }

args :: { [(Type, Name)] }
 : { [] }       -- Empty list is allowed
 | someargs { $1 }

someargs :: { [(Type, Name)] }
 : arg { [$1] }
 | someargs ',' arg { $1 ++ [$3] }

arg :: { (Type, Name) }
 : type id { ($1, $2) }

stmts :: { [Stmt] }
 : { [] }       -- Empty list is allowed
 | somestmts { $1 }

somestmts :: { [Stmt] }
 : stmt     { [$1] }
 | somestmts stmt { $1  ++ [$2] }

stmt :: { Stmt }
 : 'return' expr ';' { ReturnS $2 }
 | 'assert' expr ';' { AssertS $2 }
 | type id '=' expr ';' { blockS [DeclS $1 $2, UpdateS $2 $4] }
 | type id ';' { DeclS $1 $2 }
 | id '=' expr ';' { UpdateS $1 $3 }
 | id '[' expr ']' '=' expr ';' { ArrUpdateS $1 $3 $6 }
 | id '[' expr '::' expr ']' '=' expr ';' { ArrBulkUpdateS $1 $3 $5 $8 }
 | '{' stmts '}' { blockS $2 }
 | 'if' '(' expr ')' stmt { IfS $3 $5 (blockS [])}
 | 'if' '(' expr ')' stmt 'else' stmt { IfS $3 $5 $7 }
 | 'repeat' '(' expr ')' stmt { RepeatS $3 $5 }
 | 'while' '(' expr ')' stmt { WhileS $3 $5 }
 | 'for' '(' for_init ';' expr ';' for_incr ')' stmt { ForS $3 $5 $7 $9 }
 | ';' { blockS [] }
 | '++' id ';' { UpdateS $2 (AddE (VarE $2) (ValE $ IntV 1)) }
 | id '++' ';' { UpdateS $1 (AddE (VarE $1) (ValE $ IntV 1)) }
 | id '--' ';' { UpdateS $1 (SubE (VarE $1) (ValE $ IntV 1)) }

for_init :: { Stmt }
 : type id '=' expr { blockS [DeclS $1 $2, UpdateS $2 $4] }
 | id '=' expr { UpdateS $1 $3 }
 | id '[' expr ']' '=' expr { ArrUpdateS $1 $3 $6 }

for_incr :: { Stmt }
 : id '=' expr { UpdateS $1 $3 }
 | '++' id { UpdateS $2 (AddE (VarE $2) (ValE $ IntV 1)) }
 | id '++' { UpdateS $1 (AddE (VarE $1) (ValE $ IntV 1)) }
 | id '--' { UpdateS $1 (SubE (VarE $1) (ValE $ IntV 1)) }

expr :: { Expr }
 : '(' expr ')'     { $2 }
 | '(' type ')' expr { CastE $2 $4 }
 | expr '?' expr ':' expr { CondE $1 $3 $5 }
 | expr '&' expr    { AndE $1 $3 }
 | expr '<' expr    { LtE $1 $3 }
 | expr '>' expr    { GtE $1 $3 }
 | expr '<=' expr   { LeE $1 $3 }
 | expr '>=' expr   { GeE $1 $3 }
 | expr '==' expr   { EqE $1 $3 }
 | expr '!=' expr   { NeqE $1 $3 }
 | expr '+' expr    { AddE $1 $3 }
 | expr '-' expr    { SubE $1 $3 }
 | '-' expr         { SubE (ValE (IntV 0)) $2 }
 | expr '*' expr    { MulE $1 $3 }
 | expr '%' expr    { ModE $1 $3 }
 | expr '/' expr    { DivE $1 $3 }
 | expr '|' expr    { OrE $1 $3 }
 | expr '||' expr    { OrE $1 $3 }
 | expr '&&' expr    { AndE $1 $3 }
 | expr '^' expr    { XorE $1 $3 }
 | expr '>>' expr    { ShrE $1 $3 }
 | expr '<<' expr    { ShlE $1 $3 }
 | expr '{|}' expr   { BitChooseE UnknownT $1 $3 }
 | 'true'           { ValE (BitV True) }
 | 'false'          { ValE (BitV False) }
 | '!' expr         { NotE $2 }
 | '~' expr         { NotE $2 }
 | '??' { HoleE UnknownT Nothing }
 | '??' '(' integer ')' { HoleE UnknownT (Just $3) }
 | '{' '*' '}' { HoleE UnknownT Nothing }
 | integer { ValE (IntV $1) }
 | id { VarE $1 }
 | expr '[' expr ']' { AccessE $1 $3 }
 | expr '[' expr '::' expr ']' { BulkAccessE $1 $3 $5 }
 | '{' exprs '}' { ArrayE $2 }
 | id '(' ')' { AppE $1 [] }
 | id '(' exprs ')' { AppE $1 $3 }

exprs :: { [Expr] }
 : { [] }       -- empty list is allowed
 | someexprs { $1 }

someexprs :: { [Expr] }
 : expr { [$1] }
 | someexprs ',' expr { $1 ++ [$3] }


{

parseError :: Token -> ParserMonad a
parseError tok = do
    x <- get
    failE $ "parser error at " ++ show tok ++ "\n when parsing: " ++ x

seq :: a -> b -> b
seq = const id

}

