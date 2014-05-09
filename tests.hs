-- double arg1
doubleMe x = x + x

-- generate factorals for arg1
---- this is an example of pattern matching in haskell - if arg1 == 0 then return 1 else return n * fac (n-1)
fac 0 = 1
fac n = n * fac (n-1)

-- generate fibinacci numbers up to index arg1
fib 0 = 0
fib 1 = 1
fib x = fib(x-1) + fib(x-2)

-- generate tuples of index:item for infinite sized lists
listIndex xs = zip [0..] xs

-- remove capital letters from string 'lists'
removeCaps st = [c | c<-st, c `elem` ['a'..'z']]

-- sieve primes forever
-- primes :: [Integer]
primes y = sieve [2..]
	where 
		sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

-- given a list of tuples, produce one tuple at a time, add the two values together and return that one value
addTuples xs = [(a+b) | (a,b) <- xs]

-- whatever is fed to head, return the first element (it must be a list)
head' [] = error "won't work on empty lists"
head' (x:_) = x


-- calc bmi and display a saucy message
-- things to note
---- no single equals sign, instead a bunch of |'s
---- where bmi, binds the calculation to bmi ONCE for all condition checks
---- the tuple assignment after the where clause
---- multiple where clause must be indented somehow
bmi :: (RealFloat a) => a -> a -> String
bmi w h
	| bmi <= skinny		= "Skinny, minny"
	| bmi <= normal		= "Normal, move along"
	| bmi <= fat 		= "You're fat"
	| otherwise 		= "You're a whale"
	where 
		bmi = w / h ^ 2
		(skinny, normal, fat) = (18.5, 25.0, 30.0) 
