
-- vim: ft=haskell
{

{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Grammar (parseSketch) where

import Smten.Prelude
import Smten.Control.Monad.State
import qualified Smten.Data.Map as Map

import Lexer
import Program
import Syntax

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
    '.'     { TkDot }
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
    '+='    { TkPlusEquals }
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
    'struct' { TkStruct }
    'new' { TkNew }
    'null' { TkNull }
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

sketch :: { ([Decl], String) }
 : declorpragma { $1 }
 | sketch declorpragma {
     case ($1, $2) of
      ((d1, o1), (d2, o2)) -> (d1 ++ d2, o1 ++ " " ++ o2)
 }

declorpragma :: { ([Decl], String) }
 : decl  { ([$1], "") }
 | 'pragma' 'options' string ';' { ([], $3) }

decl :: { Decl }
 : expr id '(' args ')' '{' stmts '}'
    {% asTypeM $1 $ \ty -> FunD $2 (Function ty $4 (blockS $7)) NormalF }
 | expr id '(' args ')' 'implements' id '{' stmts '}'
    {% asTypeM $1 $ \ty -> FunD $2 (Function ty $4 (blockS $9)) (WithSpecF $7) }
 | 'generator' expr id '(' args ')' '{' stmts '}'
    {% asTypeM $2 $ \ty -> FunD $3 (Function ty $5 (blockS $8)) GeneratorF }
 | 'harness' expr id '(' args ')' '{' stmts '}'
    {% asTypeM $2 $ \ty -> FunD $3 (Function ty $5 (blockS $8)) HarnessF }
 | expr id '=' expr ';' {% asTypeM $1 $ \ty -> VarD ty $2 $4 }
 | 'struct' id '{' fields '}'   { StructD $2 $4 }

args :: { [Arg] }
 : { [] }       -- Empty list is allowed
 | someargs { $1 }

someargs :: { [Arg] }
 : arg { [$1] }
 | someargs ',' arg { $1 ++ [$3] }

arg :: { Arg }
 : expr id {% asTypeM $1 $ \ty -> Arg $2 ty False }
 | 'ref' expr id {% asTypeM $2 $ \ty -> Arg $3 ty True }

fields :: { [(Name, Type)] }
 : field        { [$1] }
 | fields field { $1 ++ [$2] }

field :: { (Name, Type) }
 : expr id ';'   {% asTypeM $1 $ \ty -> ($2, ty) }

stmts :: { [Stmt] }
 : { [] }       -- Empty list is allowed
 | somestmts { $1 }

somestmts :: { [Stmt] }
 : stmt     { [$1] }
 | somestmts stmt { $1  ++ [$2] }

stmt :: { Stmt }
 : 'return' expr ';' { ReturnS $2 }
 | 'return' ';' { ReturnS (ValE VoidV) }
 | 'assert' expr ';' { AssertS $2 }
 | expr vardecls ';' {% asTypeM $1 $ \ty -> DeclS ty $2 }
 | expr '=' expr ';' {% asLValM (flip UpdateS $3) $1 }
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
 | expr ';'    { ExprS $1 }

vardecl :: { (Name, Maybe Expr) }
 : id           { ($1, Nothing) }
 | id '=' expr  { ($1, Just $3) }

vardecls :: { [(Name, Maybe Expr)] }
 : vardecl  { [$1] }
 | vardecls ',' vardecl { $1 ++ [$3] }

initentry :: { (Name, Expr) }
 : id '=' expr  { ($1, $3 ) }

someinitentries :: { [(Name, Expr)] }
 : initentry { [$1] }
 | someinitentries ',' initentry { $1 ++ [$3] }

initentries :: { [(Name, Expr)] }
 :  { [] }
 | someinitentries { $1 }

for_init :: { Stmt }
 : expr vardecls {% asTypeM $1 $ \ty -> DeclS ty $2 }
 | id '=' expr { UpdateS (VarLV $1) $3 }
 | id '[' expr ']' '=' expr { UpdateS (ArrLV (VarLV $1) $3) $6 }

for_incr :: { Stmt }
 : id '=' expr { UpdateS (VarLV $1) $3 }
 | expr { ExprS $1 }

expr :: { Expr }
 : '(' expr ')'     { $2 }
 | '(' expr ')' expr {%
      -- $4 might be of the form (-e), in which case
      -- $2 should be treated as an expression, and this overall as
      -- a subtract expression. Otherwise $2 is a type, and this is
      -- a cast.
      case $4 of
        SubE (ValE (IntV 0)) x -> return (SubE $2 x)
        _ -> asTypeM $2 $ \ty -> CastE ty $4
    }
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
 | expr '++'         {% asLValM PostIncrE $1 }
 | expr '--'         {% asLValM PostDecrE $1 }
 | '++' expr         {% asLValM PreIncrE $2 }
 | '--' expr         {% asLValM PreDecrE $2 }
 | expr '+=' expr    {% asLValM (flip PlusEqE $3) $1 }
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
 | expr '.' id    { FieldE $1 $3 }
 | 'new' id '(' initentries ')' { NewE $2 $4 }
 | 'null' { ValE nullV }
 | '{' someexprs '}' { ArrayE $2 }
 | id '(' exprs ')' { AppE $1 $3 }
    -- The following rules allow types to be parsed as exprs:
 | 'bit' { VarE "bit" }
 | 'void' { VarE "void" }
 | 'int' { VarE "int" }
 

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

-- Note: we parse lvals as an expr to avoid reduce/reduce conflicts.
-- This converts the Expr back to an LVal, failing if it doesn't convert.
asLValM :: (LVal -> a) -> Expr -> ParserMonad a
asLValM f x = case asLVal x of
                Just lv -> return $ f lv
                Nothing -> fail $ "expected lval, but got: " ++ show x

-- Note: we parse types as an expr to avoid reduce/reduce conflicts.
-- This converts the Expr back to an Type, failing if it doesn't convert.
asTypeM :: Expr -> (Type -> a) -> ParserMonad a
asTypeM x f = case asType x of
                Just ty -> return $ f ty
                Nothing -> fail $ "expected type, but got: " ++ show x


}

