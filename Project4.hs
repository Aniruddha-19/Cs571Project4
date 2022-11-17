
type Name = String
type Age = Int
type Department = String
type Salary = Float

data Employee = Employee {
  emplName :: Name,
  emplAge :: Age,
  emplDept :: Department,
  emplSalary :: Salary
} deriving ( Eq, Show )

employees :: [Employee]
employees = [
    Employee "tom" 33 "cs" 85000.00,
    Employee "joan" 23 "ece" 110000.00,
    Employee "bill" 29 "cs" 69500.00,
    Employee "john" 28 "me" 58200.00,
    Employee "sue" 19 "cs" 22000.00
  ]

deptEmployees :: [Employee] -> Department -> [Employee]
deptEmployees [] _ = []
deptEmployees (x:xs) d
  | (emplDept x) == d = x :  deptEmployees xs d
  | otherwise = deptEmployees xs d

--#2
comprDeptEmployees :: [Employee] -> Department -> [Employee] 
comprDeptEmployees x dept  = [y | y <- x, emplDept y == dept]

--#3
employeesSalarySum empls = sum (map emplSalary empls)


--#4
quadraticRoots :: Float -> Float -> Float -> (Float,Float)  
quadraticRoots a b c = if d < 0 then error "0" else (x, y)
                        where
                          x = e + sqrt d / (2 * a)
                          y = e - sqrt d / (2 * a)
                          d = b * b - (4 * a * c)
                          e = - b / (2 * a)
--#5
expn :: (Floating a) => a -> [a]
expnCalc :: (Fractional a) => a -> a -> a -> a -> [a]

expnCalc x num deno f = v:expnCalc x newnum newdeno newf
                        where
                            v = num/deno
                            newnum = num * x
                            newdeno = deno * f
                            newf = f + 1

expn x = expnCalc x 1.0 1.0 1


--#6
charIndexes str ch = [ y | (x, y) <- zip str [0..], x == ch ]

charIndexes s c = map snd (filter (\(x,y) -> x == c) (zip s [0..]))

--#7

foldTree :: (tree1 -> tree -> tree1 -> tree1) -> (tree -> tree1) -> Tree tree -> tree1 

foldTree treeFn leafFn tree = case tree of 
    Leaf n -> leafFn n
    Tree l n r -> treeFn (foldTree treeFn leafFn l) n (foldTree treeFn leafFn r)
    

--#8. 

foldTree :: (tree1 -> tree -> tree1 -> tree1) -> (tree -> tree1) -> Tree tree -> tree1 

foldTree treeFn leafFn tree = case tree of 
    Leaf n -> leafFn n
    Tree l n r -> treeFn (foldTree treeFn leafFn l) n (foldTree treeFn leafFn r) 


flattenTree :: Tree a -> [a]
flattenTree tree = foldTree (\left v right -> left ++ [v] ++ right) (\v -> [v]) t

--#9
catenateTreeLists :: IntegerTree -> [Integer] | [Char]
catenateTreeLists = go []
    where go tl (Leaf n) = n : tl
          go tl (Node l n r) = go (n : go tl r) l


