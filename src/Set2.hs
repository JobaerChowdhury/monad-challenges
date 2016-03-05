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
lookupMay a ((x,y):xs) | (a == x) = (Just y) 
					   | otherwise = (lookupMay a xs)

minimumMay :: Ord a => [a] -> Maybe a	
minimumMay [] = Nothing 
minimumMay xs = Just (foldl1 min xs)

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay a b | (b == 0) = Nothing 
           | otherwise = Just (a / b)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing 
maximumMay xs = Just (foldl1 max xs)

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
queryGreek2 gd k = link (lookupMay k gd) (\ls -> 
	link (headMay ls) (\h -> 
		link (tailMay ls) (\t -> 
			link (maximumMay t) (\m -> 
				divMay (fromIntegral m) (fromIntegral h)
				)
			)	
		)
	)

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries ss a b = link (lookupMay a ss) (\s1 -> 
	link (lookupMay b ss) (\s2 -> 
		mkMaybe (s1 + s2))) 

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c 
yLink f ma mb = link ma (\a -> link mb (\b -> mkMaybe (f a b))) 

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 ss a b = yLink (+) (lookupMay a ss) (lookupMay b ss)


mkMaybe :: a -> Maybe a
mkMaybe a = Just a 

tailProd :: Num a => [a] -> Maybe a 
tailProd ls = transMaybe product (tailMay ls)

tailSum :: Num a => [a] -> Maybe a 
tailSum ls = transMaybe sum (tailMay ls)

transMaybe :: (a -> b) -> Maybe a -> Maybe b 
transMaybe f ma = link ma (\a -> mkMaybe (f a)) 

tailMax :: Ord a => [a] -> Maybe a 
tailMax ls = combine (transMaybe maximumMay (tailMay ls)) 

tailMin :: Ord a => [a] -> Maybe a 
tailMin ls = combine (transMaybe minimumMay (tailMay ls))

combine :: Maybe (Maybe a) -> Maybe a 
combine Nothing = Nothing 
combine (Just x) = x  
