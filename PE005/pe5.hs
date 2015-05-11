--greatedCommonDividor, eulid's algorithm
gcdy :: Integer -> Integer -> Integer
gcdy x y
	| y == 0 = x
	| True = gcdy y (x `mod` y)

--lowestCommonMultiple
lcmy :: Integer -> Integer -> Integer
lcmy x y = x * y `div` (gcdy x y)

solve :: Integer -> Integer -> Integer
solve x y
	| y == 1 = x
	| True = solve (lcmy x y) (y - 1)

main = print (solve 20 19)