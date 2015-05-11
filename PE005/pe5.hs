--not using any strings or string manipulation functions
--round down to the nearest power of 10
sizeOf :: (Integer, Integer) -> Integer
sizeOf (x, y)
	| x `div` y == 0 = y `div` 10
	| True = sizeOf(x, y * 10)



--looks at the first and last digits of a number. if they are equal it
--calls the function again without these numbers. eg. 3003 -> 33 -> true
--special case for when there's 2 or fewer dihits (ie le-ss than 100)
isPalindrome :: Integer -> Bool
isPalindrome (x)
	| x == 0 = True
	| x < 100 && (x `div` sizeOf(x, 1) == x `mod` 10) = True
	| x `div` sizeOf(x, 1) == x `mod` 10 =  isPalindrome(x `mod` sizeOf(x, 1) `div` 10)
	| True = False

--x is the biggest number found so far
solve :: (Integer, Integer, Integer) -> Integer
solve (x, y, z)
	| y <= 99 = x
	| z <= 99 = solve (x, y - 1, 999)
	| isPalindrome(y * z) = solve(max (y * z) x, y - 1, 999)
	| True = solve(x, y, z - 1)


--q = isPalindrome 0
q = solve (100, 999, 999)


main = print q
--lowest common denominator
--lcd