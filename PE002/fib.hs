fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

q = takeWhile (<4000000) [ fib(x) | x <- [1..]]

iseven n =  n `mod` 2 == 0

x = sum(filter (iseven) q)

--q = fib 2
main = print x