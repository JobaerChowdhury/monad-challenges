{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1Final where

import MCPrelude
import Set4 

randNumber :: Gen Integer
randNumber = Gen rand 

-- Five random numbers starting with seed 1
-- product of these should be 8681089573064486461641871805074254223660
fiveRands :: [Integer]
fiveRands = evalGen (sequence $ replicate 5 randNumber) (mkSeed 1)

randEven :: Gen Integer
randEven = fmap (*2) randNumber

randOdd :: Gen Integer 
randOdd = fmap (+1) randEven

randTen :: Gen Integer
randTen = fmap (*10) randNumber

randLetter :: Gen Char
randLetter = fmap toLetter randNumber

-- random string of three letters starting with seed 1 
randString3 :: String 
randString3 = evalGen (sequence $ replicate 3 randLetter) (mkSeed 1)

randPair :: Gen (Char, Integer)
randPair = liftM2 (,) randLetter randNumber
