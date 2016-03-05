{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

type Gen a = Seed -> (a, Seed)

fiveRands :: [Integer]
fiveRands = [a, b, c, d, e]
	where 
		(a, s1) = rand (mkSeed 1)
		(b, s2) = rand s1
		(c, s3) = rand s2
		(d, s4) = rand s3
		(e, _) = rand s4

randEven :: Gen Integer
randEven = generalA (* 2) rand 

randOdd :: Gen Integer 
randOdd = generalA (+ 1) randEven

randTen :: Gen Integer
randTen = generalA (* 10) rand 

generalA :: (a -> b) -> Gen a -> Gen b 
generalA f g = \x -> let (a, s1) = g x in (f a, s1)


randLetter :: Gen Char
randLetter s = (l, s2)
	where 
		(i, s2) = rand s
		l = toLetter i

randString3 :: String 
randString3 = [a, b, c]
	where 
		(a, s1) = randLetter (mkSeed 1)
		(b, s2) = randLetter s1 
		(c, _)  = randLetter s2	

randPair :: Gen (Char, Integer)
randPair s = ((c, i), s2)
	where 
		(c, s1) = randLetter s 
		(i, s2)  = rand s1 
 
generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair f g = \x -> let (a, s1) = f x in (let (b, s2) = g s1 in ((a,b), s2)) 

generalB :: Gen a -> Gen b -> (a -> b -> c) -> Gen c 
generalB f g h = \x -> let (a, s1) = f x in (let (b, s2) = g s1 in ((h a b), s2)) 

generalB2 :: Gen a -> Gen b -> (a -> b -> c) -> Gen c
generalB2 ga gb f = genTwo ga (\a -> genTwo gb (\b -> mkGen (f a b)))

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 g h = generalB g h (,)

repRandom :: [Gen a] -> Gen [a]
repRandom gs = \x -> foldl comb1 ([], x) gs  

-- try implementing it using generalA, genTwo and mkGen 
-- generalA :: (a -> b) -> Gen a -> Gen b 
-- genTwo :: Gen a -> (a -> Gen b) -> Gen b 
repRandom2 :: [Gen a] -> Gen [a]
repRandom2 = undefined 

comb1 :: ([a], Seed) -> Gen a -> ([a], Seed)
comb1 (ls, s) f = let (n, s1) = f s in (ls ++ [n], s1) 


genTwo :: Gen a -> (a -> Gen b) -> Gen b 
genTwo g f = \s -> let (r, s1) = g s in (f r) s1 

mkGen :: a -> Gen a 
mkGen x = \s -> (x, s) 
