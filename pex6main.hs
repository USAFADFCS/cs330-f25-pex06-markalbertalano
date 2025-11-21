-- pex6.hs 
-- unKnot Haskell

-- name: mark alano

{- DOCUMENTATION: instructor provided code during EI under unknot | bumbed a test script from the general teams channel
-}
unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = "not a knot"
   | t1exists tripCode = unKnot (t1move tripCode)
   | t1exists (wrap tripCode) = unKnot (t1move (wrap tripCode))
   | t2exists tripCode = unKnot (t2move tripCode)
   | t2exists (wrap tripCode) = unKnot (t2move (wrap tripCode))
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)

--func def

--wrap
wrap :: [(Char, Char)] -> [(Char, Char)]
--cond: empty, one element
wrap [] = []
wrap [x] = [x]
   --cond: possible wrap
wrap (x:xs) = wrapLast x xs

--wrap helper: element tracker
wrapLast :: (Char,Char) -> [(Char, Char)] -> [(Char, Char)]
   --base case: one element left -> can wrap
wrapLast e1 [y] = [y, e1]
   --recursive step: check until at end
wrapLast e1 (y:ys) = y : wrapLast e1 ys
--wrap tripCode = undefined


--type 1
t1exists  :: [(Char, Char)] -> Bool
   --check: empty list, one element list
t1exists [] = False
t1exists [_] = False
   --check: adj pair
t1exists ((cs1, ct1):(cs2, ct2):xs)
   | cs1 == cs2 = True
      --check: next adj pair
   | otherwise = t1exists ((cs2, ct2):xs)
--t1exists tripCode = undefined

t1move :: [(Char, Char)] -> [(Char, Char)]
   --cond: empty list, one element list
t1move [] = []
t1move [x] = [x]
   --cond: adj pair
t1move ((cs1, ct1):(cs2, ct2):xs)
   | cs1 == cs2 = xs
      --cond: next adj pair
   | otherwise = (cs1,ct1) : t1move ((cs2, ct2):xs)
--t1move tripCode = undefined


--type2
t2exists  :: [(Char, Char)] -> Bool
--check: empty, one element, two, three
t2exists [] = False
t2exists [_] = False
t2exists [_, _] = False
t2exists [_, _, _] = False
   --check dopple
t2exists ((cs1,ct1):(cs2,ct2):xs)
   | ct1 == ct2 && t2match (cs1, cs2, ct1) xs = True
   | otherwise = t2exists ((cs2, ct2):xs)

--type2exists helper
t2match :: (Char, Char, Char) -> [(Char, Char)] -> Bool
t2match (csa1, csa2, dt1) [] = False
t2match (csa1, csa2, dt1) [_] = False

t2match (csa1, csa2, dt1) ((csb1, ctb1):(csb2, ctb2):xs)
   --cond: pairs match types
   | ctb1 /= ctb2 = t2match (csa1, csa2, dt1) ((csb2, ctb2):xs)
   --cond: diff pair types
   | ctb1 == dt1 = t2match (csa1, csa2, dt1) ((csb2, ctb2):xs)
   --cond: match crosses
   | (csb1 == csa1 && csb2 == csa2) || (csb1 == csa2 && csb2 == csa1) = True
   --cond: next pair
   | otherwise = t2match (csa1, csa2, dt1) ((csb2, ctb2):xs)
--t2exists tripCode = undefined

t2move :: [(Char, Char)] -> [(Char, Char)]
--cond: possible t2
t2move [] = []
t2move [x] = [x]
t2move [x, y] = [x, y]
t2move [x, y, z] = [x, y, z]

t2move ((cs1, ct1):(cs2, ct2):xs)
   --cond: match pair type
   | ct1 == ct2 && t2match (cs1, cs2, ct1) xs = t2del (cs1, cs2, ct1) xs
   --cond: next pair
   | otherwise = (cs1, ct1) : t2move ((cs2, ct2):xs)

t2del :: (Char, Char, Char) -> [(Char, Char)] -> [(Char, Char)]
--cond: type2 possible
t2del (csa1, csa2, cta1) [] = []
t2del (csa1, csa2, cta1) [x] = [x]

t2del (csa1, csa2, cta1) ((csb1, ctb1):(csb2, ctb2):xs)
   --cond: match pair types, keep first
   | ctb1 /= ctb2 = (csb1, ctb1) : t2del (csa1, csa2, cta1) ((csb2, ctb2):xs)
   --cond: dont qualify
   | ctb1 == cta1 = (csb1, ctb1) : t2del (csa1, csa2, cta1) ((csb2, ctb2):xs)
   --cond: match
   | (csb1 == csa1 && csb2 == csa2) || (csb1 == csa2 && csb2 == csa1) = xs
   --cond: match cross pair
   | otherwise = (csb1, ctb1) : t2del (csa1, csa2, cta1) ((csb2, ctb2):xs) 
--t2move tripCode = undefined

main :: IO ()
main = do

   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)

