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
allCombs _ [] _ = []
allCombs _ _ [] = []
allCombs f (a:as) bs = (placeOne f a bs) ++ (allCombs f as bs)

placeOne :: (a -> b -> c) -> a -> [b] -> [c]
placeOne _ _ [] = []
placeOne f x (y:ys) = [f x y] ++ (placeOne f x ys)

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 = undefined 

