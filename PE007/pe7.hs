--returns true is y is divisible by any number in x
isDivisibleBy :: [Integer] -> Integer -> Bool
isDivisibleBy [] y = False
isDivisibleBy (x:xs) y = (y `mod` x == 0) || isDivisibleBy (xs) y 

solve :: [Integer] -> Integer -> Integer
solve a b
	| length a == 2 = head a
	| isDivisibleBy a b = solve a (b + 1)
	| True = solve (b:a) (b + 1)

main = print (solve [2] 3)