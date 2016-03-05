{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

allPairs :: [a] -> [b] -> [(a,b)]
allPairs as bs = allCombs (,) as bs

data Card = Card Int String 

instance Show Card where
	show (Card i s) = (show i) ++ s 

allCards :: [Int] -> [String] -> [Card]
allCards is ss = allCombs Card is ss 

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f as bs = concat $ map (\b -> combStep afs (replicate n b)) bs  
	where 
		afs = map (\a -> f a) as
		n = length afs  

-- todo: it should produce the output in correct order 
allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f as bs cs = concat $ map (\c -> combStep bfs (replicate (length bfs) c)) cs   
	where 
		afs = map (\a -> f a) as 
		bfs = concat $ map (\b -> combStep afs (replicate (length afs) b)) bs 

combStep :: [a -> b] -> [a] -> [b]
combStep fs as = zipWith (\ f a -> f a) fs as