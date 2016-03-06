{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude
import Set4 

headMay :: [a] -> Maybe a
headMay [] = Nothing 
headMay (x:xs) = Just x 

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing 
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing 
lookupMay k ((a,b):xs) | k == a = Just b 
					   | otherwise = lookupMay k xs  

minimumMay :: Ord a => [a] -> Maybe a	
minimumMay [] = Nothing
minimumMay xs = return $ foldl1 min xs 

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay a b | b == 0 = Nothing 
		   | otherwise = Just (a / b)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay xs = return $ foldl1 max xs 

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd k = bind (lookupMay k gd) (\ls -> 
	bind (tailMay ls) (\t -> 
		bind (maximumMay t) (\m -> 
			bind (headMay ls) (\h -> 
				divMay (fromIntegral m) (fromIntegral h))))) 

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries ss a b = liftM2 (+) (lookupMay a ss) (lookupMay b ss)

tailProd :: Num a => [a] -> Maybe a 
tailProd ls = bind (tailMay ls) (\l -> return $ product l) 

tailSum :: Num a => [a] -> Maybe a 
tailSum ls = bind (tailMay ls) (\l -> return $ sum l)

tailMax :: Ord a => [a] -> Maybe a 
tailMax ls = bind (tailMay ls) (\l -> maximumMay l)

tailMin :: Ord a => [a] -> Maybe a 
tailMin ls = bind (tailMay ls) (\l -> minimumMay l) 
