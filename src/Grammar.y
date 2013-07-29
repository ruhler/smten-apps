-- vim: ft=haskell
{

{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Grammar (parseHampi) where

import Smten.Prelude
import Smten.Control.Monad.State
import qualified Smten.Data.Map as Map

import Fix
import CFG
import Hampi
import Lexer

}

%name parseHampi hampi
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
    ','     { TkComma }
    ';'     { TkSemicolon }
    ':'     { TkColon }
    ':='    { TkColonEquals }
    '..'    { TkDoubleDot }
    'val'   { TkVal }
    'var'   { TkVar }
    'cfg'   { TkCfg }
    'reg'   { TkReg }
    'fix'   { TkFix }
    'star'   { TkStar }
    'or'   { TkOr }
    'not'   { TkNot }
    'assert'    { TkAssert }
    'concat'    { TkConcat }
    'in'    { TkIn}
    'contains'    { TkContains}
    'equals'    { TkEquals }
    id      { TkID $$ }
    int     { TkInt $$ }
    string  { TkString $$ }
    char  { TkChar $$ }

%%

hampi :: { Hampi }
 : vardecl ';' stmts ';'
    { mkhampi $1 $3 }

stmts :: { [Stmt] }
 : stmt
    { [$1] }
 | stmts ';' stmt
    { $1 ++ [$3] }

stmt :: { Stmt }
 : cfgstmt { $1 }
 | regstmt { $1 }
 | valstmt { $1 }
 | assertstmt { $1 }

vardecl :: { Var }
 : 'var' id ':' int
    { Var $2 $4 $4 }
 | 'var' id ':' int '..' int
    { Var $2 $4 $6 }

valstmt :: { Stmt }
 : 'val' id ':=' expr
    { valS $2 $4 }

cfgstmt :: { Stmt }
 : 'cfg' id ':=' cfgprods
    { cfgS $2 (orsC $4) }
 | 'cfg' id ':=' cfgprods '|'
    { cfgS $2 (orsC ($4 ++ [epsilonC])) }

cfgprods :: { [CFG] }
 :          { [epsilonC] }
 | cfgprod  { [$1] }
 | cfgprods '|' cfgprod { $1 ++ [$3] }

cfgprod :: { CFG }
 : cfgelems { concatsC $1 }

cfgelems :: { [CFG] }
 : cfgelem { [$1] }
 | cfgelems cfgelem { $1 ++ [$2] }

cfgelem :: { CFG }
 : string { stringC $1 }
 | char { charC $1 }
 | id { varC $1 }
 | '(' id ')' '*'
   { starC (varC $2) }
 | '(' id ')' '+'
   { plusC (varC $2) }
 | '(' id ')' '?'
   { optionC (varC $2) }
 | '[' char '-' char ']'
   { rangeC $2 $4 }

regstmt :: { Stmt }
 : 'reg' id ':=' regdef
    { cfgS $2 $4 }

regdef :: { CFG }
 : id { varC $1 }
 | string { stringC $1 }
 | char { charC $1 }
 | '[' char '-' char ']'
    { rangeC $2 $4 }
 | 'fix' '(' id ',' int ')'
    { fixC $3 $5 }
 | 'star' '(' regdef ')'
    { starC $3 }
 | 'or' '(' regdefs ')'
    { orsC $3 }
 | 'concat' '(' regdefs ')'
    { concatsC $3 }

regdefs :: { [CFG] }
 : regdef { [$1] }
 | regdefs ',' regdef { $1 ++ [$3] }
 
assertstmt :: { Stmt }
 : 'assert' id not 'in' id
    { assertInS $2 $3 $5 }
 | 'assert' id not 'contains' string
    { assertContainsS $2 $3 $5 }
 | 'assert' id not 'equals' id
    { assertEqualsS $2 $3 $5 }

not :: { Bool }
 :  { True }
 |  'not' { False } 

expr :: { Val }
 : string { stringV $1 }
 | id { idV $1 }
 | 'concat' '(' exprs ')'
    { concatV $3 }
 | id '[' int ':' int ']'
    { subV $1 $3 $5 }

exprs :: { [Val] }
 : expr { [$1] }
 | exprs ',' expr { $1 ++ [$3] }

{

data Stmt = 
   AssertStmt Assertion
 | AssertInStmt ID Bool ID
 | ValStmt ID Val
 | CfgStmt ID CFG

cfgS :: ID -> CFG -> Stmt
cfgS = CfgStmt

valS :: ID -> Val -> Stmt
valS = ValStmt

assertInS :: ID -> Bool -> ID -> Stmt
assertInS x n y = AssertInStmt x n y

assertContainsS :: ID -> Bool -> String -> Stmt
assertContainsS x n y = AssertStmt $ AssertContains x n y

assertEqualsS :: ID -> Bool -> ID -> Stmt
assertEqualsS x n y = AssertStmt $ AssertEquals x n y

mkhampi :: Var -> [Stmt] -> Hampi
mkhampi v stmts =
 let vals = [(id, v) | ValStmt id v <- stmts]
     cfgs = Map.fromList [(id, r) | CfgStmt id r <- stmts]


     mkinassert :: ID -> Bool -> ID -> Assertion
     mkinassert x n y = AssertIn x n (fixN cfgs y)

     easyasserts = [a | AssertStmt a <- stmts]
     inasserts = [mkinassert x n y | AssertInStmt x n y <- stmts]

     asserts = easyasserts ++ inasserts
 in Hampi v vals asserts

parseError :: Token -> ParserMonad a
parseError tok = do
    x <- get
    failE $ "parser error at " ++ show tok ++ "\n when parsing: " ++ x

}

