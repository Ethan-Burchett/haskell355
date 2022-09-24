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


--drop_front (a,b) xs = r_drop (fst (a,b)) xs
--drop_back (a,b) xs = reverse (r_drop (length xs) (-) (snd (a,b))) (reverse xs))
--range_helper (a,b) xs = r_drop (fst (a,b)) (reverse (r_drop (snd (a,b)) (reverse xs)))

r_drop 0 xs = xs
r_drop x xs = r_drop (x-1) (tail xs)

range_helper (a,b) xs = r_drop (fst (a,b)) (reverse (r_drop ((length xs) - snd (a,b) - 1) (reverse xs)))

sum_range (a,b) xs = sum (range_helper (a,b) xs )


-- need to calculate length - num to drop from end 
--        length - b = index of end of range
-- or 

--ultra compact version (incorrect)
--sum_range (a,b) xs = sum (drop (fst (a,b)) (reverse (drop (snd (a,b)) (reverse xs))))


-- natSum n | n == 0 = 0
--          | n > 0 = n + natSum (n - 1)
--          | otherwise = error "natSum: Input value is negative!"

-- nthElement [] n = error "nthElement': The input list is too short."
-- nthElement (x:xs) 1 = x
-- nthElement (x:xs) n = (nthElement xs (n-1))

-- init - return all the values in a array except for the last one. 
-- tail - return all the values in an array except for the first one


-- P3  (a) calc_collatz_seq ; 10%

-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)

next_even :: Integral a => a -> a
next_even n = div n 2
next_odd n = 3 * n + 1

next_collatz n | even n = next_even n
               | odd n = next_odd n

calc_collatz_seq :: Integral a => a -> [a]
calc_collatz_seq 1 = [1]
calc_collatz_seq n = n:calc_collatz_seq (next_collatz n)


longest_list xs ys | length xs > length ys = xs
                   | otherwise = ys


-- just generate all of the lists from 1 and n first, then find the longest

--longest_collatz_seq 0 = 
--longest_collatz_seq n = 

--collatz_helper 0 = []
--collatz_helper 0 = 

--helper 0 = []
--helper n [] = helper (n-1 ) calc_collatz_seq 

--countdown (0) = []
--countdown n = n:countdown (n - 1)

--make_pair n = [n, length (calc_collatz_seq n)] -- number, length

--make_tuple_array n = 

--helper n = map make_tuple (coundown n)

--get_tuple_list n = map make_pair [1..n]

--longest_collatz_seq n =  longest_list (calc_collatz_seq n) longest_collatz_seq n

-- P3  (b) longest_collatz_seq ; 15%

longest_collatz_seq :: Integral a => a -> [a]
longest_collatz_seq 1 = [1]
longest_collatz_seq n = longest_list (longest_collatz_seq (n-1)) (calc_collatz_seq n)






-- P4  (a) game_scores ; 15%


-- P4  (b) wins_by_year ; 10%


-- P5  compress_str ; 15% 


-- Assignment rules ; 4%
-- Your own tests ; 6%

