-- vim: ft=haskell
{

module Grammar (parseHampi) where

import Control.Monad.State
import Data.List (partition)
import qualified Data.Map as Map

import RegEx
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
    '|'     { TkBar }
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
    'assert'    { TkAssert }
    'concat'    { TkConcat }
    'in'    { TkIn}
    'contains'    { TkContains}
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
    { Var $2 $4 }

valstmt :: { Stmt }
 : 'val' id ':=' expr
    { valS $2 $4 }

cfgstmt :: { Stmt }
 : 'cfg' id ':=' cfgprods
    { regS $2 (orR $4) }
 | 'cfg' id ':=' cfgprods '|'
    { regS $2 (orR ($4 ++ [epsilonR])) }

cfgprods :: { [RegEx Elem] }
 :          { [] }
 | cfgprod  { [$1] }
 | cfgprods '|' cfgprod { $1 ++ [$3] }

cfgprod :: { RegEx Elem }
 : cfgelems { concatsR $1 }

cfgelems :: { [RegEx Elem] }
 : cfgelem { [$1] }
 | cfgelems cfgelem { $1 ++ [$2] }

cfgelem :: { RegEx Elem }
 : string { stringR $1 }
 | char { charR $1 }
 | id { varR $1 }

regstmt :: { Stmt }
 : 'reg' id ':=' regdef
    { regS $2 $4 }

regdef :: { RegEx Elem }
 : id { varR $1 }
 | string { stringR $1 }
 | 'fix' '(' id ',' int ')'
    { fixR $3 $5 }
 | 'star' '(' regdef ')'
    { starR $3 }
 | 'or' '(' regdefs ')'
    { orR $3 }

regdefs :: { [RegEx Elem] }
 : regdef { [$1] }
 | regdefs ',' regdef { $1 ++ [$3] }
 
assertstmt :: { Stmt }
 : 'assert' id 'in' id
    { assertInS $2 $4 }
 | 'assert' id 'contains' string
    { assertContainsS $2 $4 }

expr :: { Val }
 : string { stringV $1 }
 | id { idV $1 }
 | 'concat' '(' exprs ')'
    { concatV $3 }

exprs :: { [Val] }
 : expr { [$1] }
 | exprs ',' expr { $1 ++ [$3] }

{

data Stmt = 
   AssertStmt Assertion
 | ValStmt ID Val
 | RegStmt ID (RegEx Elem)

regS :: ID -> RegEx Elem -> Stmt
regS = RegStmt

valS :: ID -> Val -> Stmt
valS = ValStmt

assertInS :: ID -> ID -> Stmt
assertInS x y = AssertStmt $ Assert x True y

assertContainsS :: ID -> String -> Stmt
assertContainsS = error $ "TODO: assertContainsS"

mkhampi :: Var -> [Stmt] -> Hampi
mkhampi v stmts =
 let vals = Map.fromList [(id, v) | ValStmt id v <- stmts]
     regs = Map.fromList [(id, r) | RegStmt id r <- stmts]
     asserts = [a | AssertStmt a <- stmts]
 in Hampi v vals regs asserts

parseError :: Token -> ParserMonad a
parseError tok = do
    x <- get
    failE $ "parser error at " ++ show tok ++ "\n when parsing: " ++ x

}

