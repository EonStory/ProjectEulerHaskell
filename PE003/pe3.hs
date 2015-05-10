--600851475143
increment :: Integer -> Integer -> Integer
increment x y = if x `mod` y == 0 
	then y
	else y + 1

primeFactors :: Integer -> Integer
bob = 4
primeFactors x = bob

q = primeFactors 66

main = print q