-- CptS 355 - Fall 2022 -- Homework1 - Haskell
-- Name:
-- Collaborators: Jake B

module HW1
     where

-- P1 - merge_sorted 10%

--input:two sorted lists
-- merges elements 
-- returns: merged sorted list

merge_sorted :: Ord a => [a] -> [a] -> [a]
merge_sorted [] xs = xs
merge_sorted xs [] = xs
merge_sorted (x:xs) (y:ys) | x <= y = x:merge_sorted xs(y:ys) --otherwise = y:merge_sorted ys(x:xs)
                           | x >= y = y:merge_sorted ys(x:xs)


-- condition x | x > 5 = "greater than five"
--             | x < 5 = "less than five"
--             | x == 5 = "equals five"

-- P2  sum_range  15%
-- input : (x,y) [a,b,c,d]
     -- index range , list to sum. 
     -- components: function to sum items in list
     -- function to call based on the range

-- sum_range - just a sum function over the range_helper list

-- drop x ->reverse -> drop y -> reverse -> sum list

-- given a range and a list -> return a list of the given range
     
drop_front (a,b) xs = drop (fst (a,b)) xs
drop_back (a,b) xs = reverse (drop (snd (a,b)) (reverse xs))

--range_helper (a,b) xs = drop_front (a,b) (drop_back (a,b) xs)

range_helper (a,b) xs = drop (fst (a,b)) (reverse (drop (snd (a,b)) (reverse xs)))

--sum_range :: (Num p, Num a) => (a, a) -> [p] -> p
--sum_range (a,b) xs = sum (range_helper (a,b) xs )

-- or 
sum_range (a,b) xs = sum (drop (fst (a,b)) (reverse (drop (snd (a,b)) (reverse xs))))





natSum n | n == 0 = 0
         | n > 0 = n + natSum (n - 1)
         | otherwise = error "natSum: Input value is negative!"

nthElement [] n = error "nthElement': The input list is too short."
nthElement (x:xs) 1 = x
nthElement (x:xs) n = (nthElement xs (n-1))

-- init - return all the values in a array except for the last one. 
-- tail - return all the values in an array except for the first one





-- P3  (a) calc_collatz_seq ; 10%


-- P3  (b) longest_collatz_seq ; 15%


-- P4  (a) game_scores ; 15%


-- P4  (b) wins_by_year ; 10%


-- P5  compress_str ; 15% 


-- Assignment rules ; 4%
-- Your own tests ; 6%

