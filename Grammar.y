-- vim: ft=haskell
{

module Grammar (hampi) where

import Control.Monad.State
import Data.List (partition)
import qualified Data.Map as Map

import CFG
import Hampi

import Lexer

}

%name hampi hampi
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
    { fixedV $2 $4 }
 | 'var' id ':' int '..' int
    { boundedV $2 $4 $6 }

valstmt :: { Stmt }
 : 'val' id ':=' expr
    { valS $2 $4 }

cfgstmt :: { Stmt }
 : 'cfg' id ':=' cfgprods
    { valS $2 (unionC $4) }
 | 'cfg' id ':=' cfgprods '|'
    { valS $2 (unionC ($4 ++ [emptyC])) }

cfgprods :: { [CFG] }
 :          { [] }
 | cfgprod  { [$1] }
 | cfgprods '|' cfgprod { $1 ++ [$3] }

cfgprod :: { CFG }
 : cfgelems { concatC $1 }

cfgelems :: { [CFG] }
 : cfgelem { [$1] }
 | cfgelems cfgelem { $1 ++ [$2] }

cfgelem :: { CFG }
 : string { stringC $1 }
 | char { charC $1 }
 | id { varC $1 }

regstmt :: { Stmt }
 : 'reg' id ':=' regdef
    { valS $2 $4 }

regdef :: { CFG }
 : id { varC $1 }
 | string { stringC $1 }
 | 'fix' '(' id ',' int ')'
    { fixC $3 $5 }
 | 'star' '(' regdef ')'
    { starC $3 }

 
assertstmt :: { Stmt }
 : 'assert' id 'in' id
    { assertInS $2 $4 }
 | 'assert' id 'contains' string
    { assertContainsS $2 $4 }

expr :: { CFG }
 : string { stringC $1 }
 | id { varC $1 }
 | 'concat' '(' exprs ')'
    { concatC $3 }

exprs :: { [CFG] }
 : expr { [$1] }
 | exprs ',' expr { $1 ++ [$3] }

{

data Stmt = 
   AssertStmt Assertion
 | ValStmt ID CFG
    deriving (Eq, Show)

isValStmt :: Stmt -> Bool
isValStmt (ValStmt {}) = True
isValStmt _ = False

valS :: ID -> CFG -> Stmt
valS = ValStmt

assertInS :: ID -> ID -> Stmt
assertInS x y = AssertStmt $ Assertion x True In (varC y)

assertContainsS :: ID -> String -> Stmt
assertContainsS x y = AssertStmt $ Assertion x True Contains (stringC y)

mkhampi :: Var -> [Stmt] -> Hampi
mkhampi v stmts =
 let (vals, asserts) = partition isValStmt stmts
     env = Map.fromList [(k, v) | ValStmt k v <- vals]
 in Hampi v env [a | AssertStmt a <- asserts]

parseError :: Token -> ParserMonad a
parseError tok = do
    x <- get
    failE $ "parser error at " ++ show tok ++ "\n when parsing: " ++ x

}

