{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Nothing | Just a 

instance Show a => Show (Maybe a) where
	show (Just a) = "Just " ++ show a 
	show Nothing = "Nothing" 

headMay :: [a] -> Maybe a
headMay [] = Nothing 
headMay (x:xs) = Just x 

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing 
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing 
lookupMay a ((x,y):xs) = if (a == x) then (Just y) else (lookupMay a xs)

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay a b = if (b == 0) then Nothing else Just (a / b)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing 
maximumMay xs = Just (foldl1 max xs)

minimumMay :: Ord a => [a] -> Maybe a	
minimumMay [] = Nothing 
minimumMay xs = Just (foldl1 min xs)

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd k = case (lookupMay k gd) of 
					Nothing   -> Nothing
					(Just xs) -> case (tailMay xs) of 
						Nothing -> Nothing
						Just ts -> case (maximumMay ts) of 
						    Nothing -> Nothing
						    Just a  -> case (headMay xs) of 
						        Nothing -> Nothing 
						        Just h -> case (divMay (fromIntegral a) (fromIntegral h)) of 
						            Nothing -> Nothing 
						            Just r -> Just r

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing = Nothing
chain f (Just a) = f a  

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link Nothing _ = Nothing
link (Just a) f = f a 

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gd k = undefined 




