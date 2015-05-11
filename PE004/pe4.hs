sizeOf :: (Integer, Integer) -> Integer
sizeOf (x, y)
	| x `div` y == 0 = y `div` 10
	| True = sizeOf(x, y * 10)

-- avoided using strings for this
-- basic idea to to take a number, check if the first and last digits
--are equal, and if they are, send it recursively dow nto check again
-- another number, y, must be kept to keep track of trailing 0's
--to avoid causes such as 50101 being turned into 10 instead of 010.
isPalindrome :: (Integer, Integer) -> Bool  
isPalindrome (x, y)
	| x == 0 = True
	| firstDigit == lastDigit = isPalindrome(x `mod` y `div` 10, y `div` 100)
    | True = False
    where firstDigit = if (x < y) then 0 else (x `div` y)
          lastDigit = x `mod` 10
	



--x is the biggest number found so far
solve :: (Integer, Integer, Integer) -> Integer
solve (x, y, z)
	| y <= 99 = x
	| z <= 99 = solve (x, y - 1, 999)
	| isPalindrome(y * z, sizeOf(y * z, 1)) = solve(max (y * z) x, y - 1, 999)
	| True = solve(x, y, z - 1)

q = solve (1, 999, 999)
	
main = print q