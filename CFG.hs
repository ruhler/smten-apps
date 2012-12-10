
module CFG (
    CFG(..), ID,
    charC, stringC, unionC, concatC, varC, fixC, emptyC,
    ) where

type ID = String

data CFG = C_Empty
         | C_Epsilon
         | C_Atom Char Char     -- ^ Character range. May be singleton.
         | C_Star CFG
         | C_Concat [CFG]
         | C_Union [CFG]
         | C_Var ID
         | C_Fix ID Integer
  deriving(Eq, Show)

charC :: Char -> CFG
charC c = C_Atom c c

stringC :: String -> CFG
stringC = concatC . map charC

unionC :: [CFG] -> CFG
unionC [] = error "unionC on empty list"
unionC [x] = x
unionC xs = C_Union xs

concatC :: [CFG] -> CFG
concatC [] = error "TODO: concatC on empty list"
concatC [x] = x
concatC xs = C_Concat xs

varC :: ID -> CFG
varC = C_Var

fixC :: ID -> Integer -> CFG
fixC = C_Fix

emptyC :: CFG
emptyC = C_Empty

