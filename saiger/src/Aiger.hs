
module Aiger (
    Aiger, Vector, Literal,
    aig_num_inputs, aig_num_outputs, aig_num_latches, aig_outputs,
    aig_eval, aig_step, aig_reset,

    readAsciiAiger, showVector,
  ) where

import Smten.Prelude
import Smten.Data.Array

-- Notes on the Aiger format:
--  * literal 0 stands for False
--  * literal 1 stands for True
--  * literals 2*[1..num_inputs] are the positive literals corresponding to
--    input variables [1..num_inputs].
--  * literals 2*[1..num_inputs]+1 are the negative literals corresponding to
--    input variables [1..num_inputs]
--  * literals 2*[num_inputs+1..num_inputs+num_latches] are the positive
--    literals corresponding to latche variables [1..num_latches]
--  * literals 2*[num_inputs+1..num_inputs+num_latches] + 1 are the negative
--    literals corresponding to latche variables [1..num_latches]
--  * literals 2*[num_inputs+num_latches+1..num_inputs+num_latches+num_ands]
--    are the positive literals corresponding to and gate variables [1..num_ands]
--  * literals
--      2*[num_inputs+num_latches+1..num_inputs+num_latches+num_ands] + 1
--    are the negative literals corresponding to and gate variables [1..num_ands]

-- | Vectors should be 1-indexed
type Vector = Array Int Bool

data Source = Input | Latch | AndGate 
    deriving (Show)

-- | Literal p idx src
--    p - True if the literal is inverted
--    idx - The index into the corresponding source array
--          ex: idx 4 means the fourth Input, the fourth Latch, or the fourth
--          AndGate depending on the src
--    src - The source of the literal
data Literal = Literal Bool Int Source
             | TrueLiteral | FalseLiteral
    deriving (Show)

data Aiger = Aiger {
   aig_num_inputs :: Int,
   aig_num_outputs :: Int,
   aig_num_latches :: Int,
   aig_outputs :: Array Int Literal,
   aig_latches :: Array Int Literal,
   aig_andgates :: Array Int (Literal, Literal)
} deriving (Show)

-- The reset state for this aiger.
aig_reset :: Aiger -> Vector
aig_reset aig = listArray (1, aig_num_latches aig) (replicate (aig_num_latches aig) False)

-- | aig_eval
--   Evaluate literals in an aig with given input and state.
aig_eval :: Aiger -> Vector -> Vector -> (Literal -> Bool)
aig_eval aig inputs state = 
  let flit :: Literal -> Bool
      flit TrueLiteral = True
      flit FalseLiteral = False
      flit (Literal invert idx src) =
        let arr = case src of
                    Input -> inputs
                    Latch -> state
                    AndGate -> ands
            v = arr ! idx
        in if invert then not v else v

      fand :: (Literal, Literal) -> Bool
      fand (l1, l2) = flit l1 && flit l2

      ands = fmap fand (aig_andgates aig)
  in flit

-- | aig_step
--   One run step of the aig network
--  aig - the network to run
--  inputs - the input vector. Should have length (aig_num_inputs aig)
--  state - the state vector. Should have length (aig_num_latches aig)
-- Returns (output, newstate)
--   where output is a vector with length (aig_num_outputs aig),
--     and newstate is a vector with length (aig_num_latches aig)
--      
aig_step :: Aiger -> Vector -> Vector -> (Vector, Vector)
aig_step aig inputs state = 
  let flit = aig_eval aig inputs state
      outs = fmap flit (aig_outputs aig)
      newstate = fmap flit (aig_latches aig)
  in (outs, newstate)
 

-- | Parse an ascii aiger file, assuming it is restricted in the following
-- way:
--   - inputs are in order
--   - latches are in order
--   - and gates are in order
readAsciiAiger :: String -> Aiger
readAsciiAiger text =
  case (lines text) of
     [] -> error "AsciiAiger file is empty"
     (header : nonheader) ->
       case words header ++ repeat "0" of
         (fmt : _) | fmt /= "aag" -> error "AsciiAiger file is not aag format"
         (_ : m : i : l : o : a : _) ->
            let numinputs = read i
                numlatches = read l
                numoutputs = read o
                numands = read a

                (inputlines, noninput) = splitAt numinputs nonheader
                (latchlines, nonlatch) = splitAt numlatches noninput
                (outputlines, nonoutput) = splitAt numoutputs nonlatch
                (andlines, nonand) = splitAt numands nonoutput

                -- Compute the literal from its given number
                literal :: Int -> Literal
                literal 0 = FalseLiteral
                literal 1 = TrueLiteral
                literal i = 
                  let inverted = i `rem` 2 == 1
                      index = i `quot` 2
                  in case index of
                        v | v <= numinputs -> Literal inverted v Input
                          | v <= numinputs + numlatches -> Literal inverted (v - numinputs) Latch
                          | otherwise -> Literal inverted (v - numinputs - numlatches) AndGate

                mkout :: String -> Literal
                mkout = literal . read

                mklatch :: String -> Literal
                mklatch ln = case words ln of
                             [_, x] -> literal (read x)

                mkand :: String -> (Literal, Literal)
                mkand ln = case words ln of
                            [_, x, y] -> (literal (read x), literal (read y))

                outputs = listArray (1, numoutputs) (map mkout outputlines)
                latches = listArray (1, numlatches) (map mklatch latchlines)
                andgates = listArray (1, numands) (map mkand andlines)
            in Aiger { 
                 aig_num_inputs = numinputs,
                 aig_num_outputs = numoutputs,
                 aig_num_latches = numlatches,
                 aig_outputs = outputs,
                 aig_latches = latches,
                 aig_andgates = andgates
             }

-- Show a vector as a binary string.
showVector :: Vector -> String
showVector v =
  let f b = if b then '1' else '0'
  in map f (elems v)

