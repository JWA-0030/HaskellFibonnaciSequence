--Jesse Alsing
-- 5/23/18

--1.
-- so i was playing around with list comphrehensions and found 
-- that I could say takeWhile and call fib immediately then filter the evens
-- by googling around I found that I could use zipWith to add the last 
-- element of the list with the current giving us the current fib then filtering
-- to under 4 million

fibonacci = sum [x |x <- takeWhile (<= 4000000) fib, even x]
 where
   fib = 0 : 1 : zipWith (+) fib (tail fib)

--2.
-- I kinda just copied the implementation from in class for if a nummber is a factor of 3 or 7
-- then i filter for the odds

sumOfOddMults n = sum [ x | x <- [1..n], odd x, isFactor x 3, isFactor x 7]
isFactor n x = n `mod` x == 0

--3.
-- I had the most trouble with syntax on this question. I kept getting type mismatches
-- I finally had a breakthrough by defining the type of the sum of the factors 
-- then i filter for those sums that equal 8

factorsWithEight = length [x |x <- [1..104], sumFactors x == 8]

sumFactors :: Int ->Int
sumFactors n = length [x | x <- [1..n], n `mod` x == 0]

