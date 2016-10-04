module Lib
    ( someFunc
    ) where

--filter
mfilter :: (a -> Bool) -> [a] -> [a]
mfilter pred xs = [x | x <- xs, pred x]

-- Split a list into a list of lists with length size, including "leftovers"
-- that might form a list with a length < size
mpartition_inclusive :: Int -> [a] -> [[a]]
mpartition_inclusive size xs = head : (mpartition_inclusive size tail)
	where 
		head = take size xs 
		tail = drop size xs

-- Split a list into a list of lists with length size 
mpartition :: Int -> [a] -> [[a]]
mpartition size xs 
	| length xs >= size = head : (mpartition size tail)
	| otherwise = []
		where 
			head = take size xs 
			tail = drop size xs
	
-- Removed duplicate elements from a list
dedup :: (Eq a) => [a] -> [a]
dedup xs = dedupr [] xs
	where
		dedupr seen [] = seen
		dedupr seen (x:xs) 
			| elem x seen = dedupr seen xs
			| otherwise = dedupr (x:seen) xs

-- Returns the first element of each tuple in a list
keys :: (Eq b) => [(b, a)] -> [b]
keys xs = [fst x | x<-xs]

-- Returns the second element from each tuple in a list where key == the first
vals :: (Eq b) => b -> [(b, a)] -> [a]
vals key xs = [snd x | x<-xs, key == fst x]


-- GroupBy using primtive recursion
group_by :: (Eq b) => (a -> b) -> [a] -> [(b, [a])]
group_by pfn coll = [(k, vals k kvpairs) | k<-pkeys]
	where 
		partition fn [] res = res
		partition fn (x:xs) res = (fn x, x):res ++ partition fn xs res
		kvpairs = partition pfn coll []
		pkeys = (dedup . keys) kvpairs


-- GroupBy not using primtive recursion
group_by2 :: (Eq b) => (a -> b) -> [a] -> [(b, [a])]
group_by2 pfn coll = [(k, vals k pairs) | k<-pkeys]
	where 
		kvpairs :: (a -> b) -> [a] -> [(b, a)]
		kvpairs fn xs = (zip (map fn xs) xs)

		pairs = kvpairs pfn coll
		pkeys = (dedup . keys) pairs


alternate :: [a] -> [a]
alternate xs = skip xs
	where 
		take (x:xs) = x:skip xs
		take [] = []
		skip (x:xs) = take xs
		skip [] = []


someFunc :: IO ()
--someFunc = print $ mfilter (>3) $ alternate [1, 2, 3, 4, 5, 6, 7]
--someFunc = print $ mpartition 3 [1, 2, 3, 4, 5, 6, 7]

someFunc = print $ group_by2 (>3) [1, 2, 3, 4, 5, 6, 7] 
-- someFunc = print $ (dedup . keys) $ kvpairs (>3) [1, 2, 3, 4, 5, 6, 7] 
