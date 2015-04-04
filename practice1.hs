import Data.List
import Data.Ratio

--a
sub::(Num a)=>[a]->[a]->[a]
sub (x:xs) [] = x:sub xs []
sub [] (y:ys) = y:sub [] ys
sub [] [] = []
sub (x:xs) (y:ys) = (x-y):sub xs ys
{--
sub [3,2,1,9][2,3,4,1]
[1,-1,-3,8]
--}

--b
scaleList::(Num a)=>a->[a]->[a]
scaleList c lst = [c*x|x<-lst]
{--
 scaleList 2 [1,2,3,4]
[2,4,6,8]
--}

--c
subScale::(Fractional a)=>[a]->[a]->[a]
subScale lst1@(x:xs) lst2@(y:ys) = let scalFirstList (m:ms)= (scaleList (y/x) lst1) 
									in tail (sub lst2 (scalFirstList lst1))
{--
 subScale [1,2,3,4][1,5,6,7]
[3.0,3.0,3.0]  
--}

--d
nonZeroFirst::(Num a,Eq a)=>[[a]]->[[a]]
nonZeroFirst ([[]]) = error "Empty"
nonZeroFirst list   = let check lst = if all(\x-> (head x)==0) lst then error "All lists start with zeros" 
									else lst
						in (check [x|x<-list, head x /=0]++[y|y<-list, head y == 0])
  {--
   nonZeroFirst [[0,0,1,2],[1,2,3,4],[0,3,4,5]]
[[1,2,3,4],[0,0,1,2],[0,3,4,5]]
  --}

--e

triangulate :: (Fractional a, Eq a) => [[a]] -> [[a]]
triangulate list
			|length (last list) == 2 = list
			|otherwise = [head list]++(triangulate(removeZeros(nonZeroFirst(scalify (head (nonZeroFirst list)) (tail (nonZeroFirst list))))))
{--
triangulate [[2,3,3,8],[2,3,-2,3],[4,-2,2,4]]
[[2.0,3.0,3.0,8.0],[-8.0,-4.0,-12.0],[-5.0,-5.0]]
--}

scalify::(Fractional a, Eq a)=>[a]->[[a]]->[[a]]
scalify list1 [] = [] 	
scalify list1 (x:xs) = 	[(subScale list1 x)]++(scalify list1 xs)	

{--
scalify [1,2,3,4] [[1,2,3]]
[[0.0,0.0,4.0]]
--}


removeZeros::(Num a,Eq a)=>[[a]]->[[a]]
removeZeros [] = []
removeZeros (x:xs) = [(remZero x)]++removeZeros xs


remZero::(Num a, Eq a)=>[a]->[a]
remZero (x:xs) 
		| x/=0 = x:xs
		|otherwise = remZero xs 
{--remZero [0,2,3,4,0,5,6]
[2,3,4,0,5,6]--}

scale :: Fractional a => [[a]] -> [[a]]
scale (x:y:z:cs) = [x]++[(subScale x y)]++[(subScale x z)]


--f
dot::(Num a)=>[a]->[a]->a
dot [] [] = 0
dot [] _  = 0
dot _  [] = 0
dot (x:xs) (y:ys)= (x*y) + dot xs ys
{--
dot [1,2] [3,4]
11
--}

--g		
solveLine::(Fractional a)=>[a]->[a]->a
solveLine list1 list2= ((last list1)-(dot (calc (tail list1) (length (tail list1)) []) list2))/(head list1) 
{--
solveLine [2,3,3,8] [1,1]
1.0
--}

calc::(Num a, Eq a) => [a1] -> a -> [a1] -> [a1]
calc (x:xs) counter buffer
	|counter==1 = reverse buffer
	|otherwise = calc xs (counter-1) (x:buffer)
  {--
   calc [1,2,3,4] 2 [4,5,6,7]
[7,6,5,4,1]
  --}


--h 
solveTriangular::(Fractional a)=>[[a]]->[a]
solveTriangular (x:[]) = [(last x)/(head x)]
solveTriangular (x:xs) = (solveLine x (solveTriangular xs)):solveTriangular xs
{--
solveTriangular [[2.0,3.0,3.0,8.0],[-8.0,-4.0,-12.0],[-5.0,-5.0]]
[1.0,1.0,1.0]
--}

--i
solveSystem::(Fractional a,Eq a)=>[[a]]->[a]
solveSystem list = solveTriangular (triangulate list)
{--
 solveSystem [[2,5,-9,3,151],[5,6,-4,2,103],[3,-4,2,7,16],[11,7,4,-8,-32]]
 [3.0,5.0,-10.999999999999996,7.000000000000008]
--}


