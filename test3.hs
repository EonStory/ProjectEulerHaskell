--600851475143
solve :: (Integer, Integer) -> (Integer, Integer)
solve (x, y) 
	| y >= x = (x, y)
	| x `mod` y == 0 = solve (x `div` y, y)
	| True = solve (x, y + 1)

q = solve (600851475143, 2)

main = print (fst q)