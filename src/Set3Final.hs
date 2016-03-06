{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude
import Set4 


data Card = Card Int String 

instance Show Card where
	show (Card i s) = (show i) ++ s 


allPairs :: [a] -> [b] -> [(a,b)]
allPairs as bs = liftM2 (,) as bs  

allCards :: [Int] -> [String] -> [Card]
allCards is ss = liftM2 Card is ss  

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs = liftM2

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 = liftM3 