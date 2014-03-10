
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
    'do'    { TkDo }
    'else'  { TkElse }
    'bit'   { TkBit }
    'int'   { TkInt }
    'void'   { TkVoid }
    'implements'   { TkImplements }
    'return'   { TkReturn }
    'reorder'   { TkReorder }
    'assert'   { TkAssert }
    'generator'{ TkGenerator }
    'ref'   { TkRef }
    'harness'{ TkHarness }
    'true'  { TkTrue }
    'false' { TkFalse }
    'pragma' { TkPragma }
    'options' { TkOptions }
    id      { TkID $$ }
    integer { TkInteger $$ }
    string { TkString $$ }

%left '||' '?'
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

sketch :: { (Prog, String) }
 : declorpragma { $1 }
 | sketch declorpragma {
     case ($1, $2) of
      ((d1, o1), (d2, o2)) -> (d1 ++ d2, o1 ++ " " ++ o2)
 }

declorpragma :: { ([Decl], String) }
 : decl  { ([$1], "") }
 | 'pragma' 'options' string ';' { ([], $3) }

decl :: { Decl }
 : type id '(' args ')' '{' stmts '}'
    { FunD $2 (Function $1 $4 (blockS $7)) NormalF }
 | type id '(' args ')' 'implements' id '{' stmts '}'
    { FunD $2 (Function $1 $4 (blockS $9)) (WithSpecF $7) }
 | 'generator' type id '(' args ')' '{' stmts '}'
    { FunD $3 (Function $2 $5 (blockS $8)) GeneratorF }
 | 'harness' type id '(' args ')' '{' stmts '}'
    { FunD $3 (Function $2 $5 (blockS $8)) HarnessF }
 | type id '=' expr ';' { VarD $1 $2 $4 }

type :: { Type }
 : 'bit' { BitT }
 | 'void' { VoidT }
 | type '[' expr ']' { ArrT $1 $3 }
 | 'int' { IntT }

args :: { [Arg] }
 : { [] }       -- Empty list is allowed
 | someargs { $1 }

someargs :: { [Arg] }
 : arg { [$1] }
 | someargs ',' arg { $1 ++ [$3] }

arg :: { Arg }
 : type id { Arg $2 $1 False }
 | 'ref' type id { Arg $3 $2 True }

stmts :: { [Stmt] }
 : { [] }       -- Empty list is allowed
 | somestmts { $1 }

somestmts :: { [Stmt] }
 : stmt     { [$1] }
 | somestmts stmt { $1  ++ [$2] }

stmt :: { Stmt }
 : 'return' expr ';' { ReturnS $2 }
 | 'assert' expr ';' { AssertS $2 }
 | type id '=' expr ';' { blockS [DeclS $1 $2, UpdateS (VarLV $2) $4] }
 | type id ';' { DeclS $1 $2 }
 | lval '=' expr ';' { UpdateS $1 $3 }
 | 'reorder' '{' stmts '}' { ReorderS $3 }
 | '{' stmts '}' { blockS $2 }
 | 'if' '(' expr ')' stmt { IfS $3 $5 (blockS [])}
 | 'if' '(' expr ')' stmt 'else' stmt { IfS $3 $5 $7 }
 | 'repeat' '(' expr ')' stmt { RepeatS $3 $5 }
 | 'while' '(' expr ')' stmt { WhileS $3 $5 }
    -- TODO: Are we allowed to duplicate the stmt for do-while?
    -- What if it has a whole or generator in it?
 | 'do' stmt 'while' '(' expr ')' ';' { BlockS [$2, WhileS $5 $2] }
 | 'for' '(' for_init ';' expr ';' for_incr ')' stmt { ForS $3 $5 $7 $9 }
 | ';' { blockS [] }
 | '++' id ';' { UpdateS (VarLV $2) (AddE (VarE $2) (ValE $ IntV 1)) }
 | id '++' ';' { UpdateS (VarLV $1) (AddE (VarE $1) (ValE $ IntV 1)) }
 | id '--' ';' { UpdateS (VarLV $1) (SubE (VarE $1) (ValE $ IntV 1)) }

for_init :: { Stmt }
 : type id '=' expr { blockS [DeclS $1 $2, UpdateS (VarLV $2) $4] }
 | id '=' expr { UpdateS (VarLV $1) $3 }
 | id '[' expr ']' '=' expr { UpdateS (ArrLV (VarLV $1) $3) $6 }

lval :: { LVal }
 : id { VarLV $1 }
 | lval '[' expr ']' { ArrLV $1 $3 }
 | lval '[' expr '::' expr ']' { BulkLV $1 $3 $5 }

for_incr :: { Stmt }
 : id '=' expr { UpdateS (VarLV $1) $3 }
 | '++' id { UpdateS (VarLV $2) (AddE (VarE $2) (ValE $ IntV 1)) }
 | id '++' { UpdateS (VarLV $1) (AddE (VarE $1) (ValE $ IntV 1)) }
 | id '--' { UpdateS (VarLV $1) (SubE (VarE $1) (ValE $ IntV 1)) }

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
 | expr '||' expr    { LOrE $1 $3 }
 | expr '&&' expr    { LAndE $1 $3 }
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

