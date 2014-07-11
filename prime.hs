
divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ld :: Integer -> Integer 
ld n = ldf 2 n 

ldf :: Integer -> Integer -> Integer 
ldf k n | divides k n = k 
        | k^2 >=  n    = n 
        | otherwise   = ldf (k+1) n

myprime n | n < 1 = error "not positive"
          | n == 1 = False
          | otherwise = ld n == n

-- exercise 1.9
maxList :: Ord a => [a] -> a
maxList [] = error "empty list"
maxList [x] = x
maxList (x:xs) = max x $ maxList xs

-- a related function:

minList :: Ord a => [a] -> a
minList [] = error "empty list"
minList [x] = x
minList (x:xs) = min x $ minList xs

-- exercise 1.10
removeFirst :: Integral a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst m (x:xs) | m == x = xs
                     | otherwise = x: removeFirst m xs

removeAll :: Integral a => a -> [a] -> [a]
removeAll _ [] = []
removeAll m (x:xs) | m == x = removeAll m xs
                   | otherwise = x: removeAll m xs

-- exercise 1.11
sortIntList :: Integral a => [a] -> [a]
sortIntList [] = []
sortIntList xs = min : sortIntList (removeFirst min xs)
    where min = minList xs

-- exercise 1.12
average [] = error "empty list"
average xs = toRational (sum xs) / toRational (length xs)

-- exercise 1.13
occurencesOfChar :: Char -> [Char] -> Int
occurencesOfChar c [] = 0
occurencesOfChar c (x:xs) | c == x = 1 + occurencesOfChar c xs
                          | otherwise = occurencesOfChar c xs

-- I think a more idiomatic way to do the same thing
occurencesOfChar2 :: Char -> [Char] -> Int
occurencesOfChar2 c [] = 0
occurencesOfChar2 c xs = sum $ map occ xs
    where occ x | x == c = 1
                | otherwise = 0

-- exercise 1.14
blowup :: String -> String
blowup [] = error "empty string!"
blowup str = concat $ map rep $ zip indices str
    where indices = [1..(length str)]
          rep (c,n) = replicate c n

-- this is explicit and works, but is messy
-- a more elegant version (a one liner)

blowup2 :: String -> String
blowup2 [] = error "empty string"
blowup2 str = concat $ zipWith replicate [1..] str

-- exercise 1.15
-- sort a list of strings alphabetically
sortString :: [String] -> [String]
sortString [] = []
sortString (x:xs) = 
    let before  = sortString [s | s <- xs, s <= x]
        after   = sortString [s | s <- xs, s > x]
    in before ++ [x] ++ after

-- done!

-- exercise 1.17

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (y==x) && prefix xs ys


-- substring!
subString :: String -> String -> Bool
subString [] _ = True
subString _ [] = False
subString xs (y:ys) | prefix xs (y:ys) = True
                    | subString xs ys  = True
                    | otherwise        = False


-- exercise 1.18
-- making expressions of certain types
-- ghc seems to prefer [Char] to String

-- [String] || [[Char]]
mystrings = ["just","a","couple","strings"]

-- (Bool,String) || (Bool,[Char])
boolString = (True,"True")

-- [(Bool,String)] || [(Bool,[Char])]
boolStrings = [(True,"True"),(False,"False")]

-- ([Bool],String) || ([Bool],[Char])
boolListString = ([True,False],"TrueFalse")

-- Bool -> Bool
boolBool :: Bool -> Bool
boolBool False = True
boolBool True = False

-- useful! haha



-- exercise 1.19 is all in the interpreter
-- (looking at the type signature for some builtins and figuring out what they do)



-- prime factorization algorithm
-- just a simple one
-- find k prime factors of n p_i such that p_1 \cdot \ldots \cdot \p_k = n

primeFactors :: Integer -> [Integer]
primeFactors n | n < 1 = error "negative num"
               | n == 1 = []
               | otherwise = p : primeFactors (div n p)
                    where p = ld n



-- exercise 1.20
lengths :: [[a]] -> [Int]
lengths [] = [0]
lengths xs = map length xs
