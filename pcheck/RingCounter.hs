
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module RingCounter (tests) where

import Smten.Prelude
import Smten.Symbolic
import Smten.Symbolic.SMT
import Smten.Symbolic.Solver.Yices2
import PCheck

-- | 3-bit Ring Counter
data RingCounter = RingCounter { 
    a :: Bool,
    b :: Bool,
    c :: Bool
}

instance Eq RingCounter where
    (==) x y = a x == a y
            && b x == b y
            && c x == c y

instance Free RingCounter where
    free = do
        av <- free
        bv <- free
        cv <- free
        return (RingCounter av bv cv)

instance Show RingCounter where
    show x = "RingCounter { "
        ++ "a = " ++ show (a x) ++ ", "
        ++ "b = " ++ show (b x) ++ ", "
        ++ "c = " ++ show (c x) ++ "}"

initial :: RingCounter
initial = RingCounter True False False
    
transition :: RingCounter -> RingCounter -> Bool
transition x y = and [b y == a x, c y == b x, a y == c x]

model :: Model RingCounter
model = Model (== initial) transition

onehigh :: RingCounter -> Bool
onehigh x = or [a x && not (b x) && not (c x),
                not (a x) && b x && not (c x),
                not (a x) && not (b x) && c x]

notc :: RingCounter -> Bool
notc x = not (c x)

tests :: IO ()
tests = do
   s <- runSMT yices2 (pcheck model onehigh)
   case s of
      Nothing -> return ()
      Just xs -> error $ "onehigh property failed: " ++ show xs

   s <- runSMT yices2 (pcheck model notc)
   case s of
      Just xs -> putStrLn $ show xs
      Nothing -> error "notc property verified"
