{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude

class Monad m where
	bind :: m a -> (a -> m b) -> m b	
	return :: a -> m a 

instance Monad Maybe where
	bind = link 
	return = mkMaybe 

instance Monad [] where
	bind ls f = undefined  
	return a = [a]

data Maybe a = Nothing | Just a 

instance Show a => Show (Maybe a) where
	show (Just a) = "Just " ++ show a 
	show Nothing = "Nothing" 

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link Nothing _ = Nothing
link (Just a) f = f a 

mkMaybe :: a -> Maybe a
mkMaybe a = Just a 

newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

evalGen :: Gen a -> Seed -> a
evalGen ga s = let (a, x) = runGen ga s in a  

instance Monad Gen where
	return = mkGen
	bind = linkGen

mkGen :: a -> Gen a 
mkGen x = Gen(\s -> (x, s)) 

linkGen :: Gen a -> (a -> Gen b) -> Gen b 
linkGen ga f = Gen(\s -> let (a, s') = runGen ga s in runGen (f a) s')


-- now some generic Monad functions. implement them using bind and return. 

liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = bind ma (\a -> bind mb (\b -> return (f a b)))

(=<<) :: (Monad m) => (a -> m b) -> m a -> m b
(=<<) = flip bind

join :: (Monad m) => m (m a) -> m a 
join mma = bind mma (\ma -> ma)

liftM3 :: (Monad m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d 
liftM3 f ma mb mc = bind ma (\a -> liftM2 (f a) mb mc)

sequence :: (Monad m) => [m a] -> m [a]
sequence mas = foldl step (return []) mas 
	where 
		step xs mx = bind xs (\ls -> bind mx ( \x -> return (ls ++ [x]))) 

ap :: (Monad m) => m (a -> b)-> m a -> m b 
ap mf ma = bind mf (\f -> bind ma (\a -> return (f a)))
