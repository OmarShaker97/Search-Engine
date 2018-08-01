import Data.List
data Ex = Ex Float Float String String deriving Show
----------Midterms/Quizzes/Name/Pass or Fail?
data NewSt = NewSt Float Float String deriving Show
----------------Midterms/Quizzes/Name
data Dist = Dist Float NewSt Ex deriving Show
------------Distance between NewSt and Ex

euclidean :: NewSt -> Ex -> Dist
square num = num*num
euclidean (NewSt x1 y1 newname) (Ex x2 y2 exname pass) = Dist (sqrt(square(x1-x2)+square(y1-y2))) (NewSt x1 y1 newname) (Ex x2 y2 exname pass) 

manhattan :: NewSt -> Ex -> Dist
manhattan (NewSt m1 q1 n1) (Ex m2 q2 n2 per) = Dist (abs(m1-m2)+abs(q1-q2)) (NewSt m1 q1 n1) (Ex m2 q2 n2 per) 

dist :: (a -> b -> c) -> a -> b -> c
dist f a b= (f a b)

takeN n l | n==0=[]
takeN n (x:xs) |  n>0=x:takeN (n-1) xs

all_dists :: (a -> b -> c) -> a -> [b] -> [c]
all_dists f x []=[]
all_dists f x (x1:xs)=(f x x1):all_dists f x xs

--------------- sort list according to float of Dist with help of sortBy
sortN :: Dist -> Dist -> Ordering
sortN (Dist n _ _) (Dist n1 _ _) = compare (n) (n1)
closest :: (Num a, Ord a) => (b -> c -> Dist) -> a -> [c] -> b -> [Dist]
closest f num lst nstudent = takeN num (sortBy (sortN) (all_dists f nstudent lst))  

grouped_dists :: (Ord a, Num a) => (b -> c -> Dist) -> a -> [c] -> b -> [[Dist]]
grouped_dists f num extlist nstudent=(prodPass (closest f num extlist nstudent)):(prodFail (closest f num extlist nstudent)):[]
------------ produce a list of Dist where label(pass or fail) of Ex of Dist is only pass
prodPass:: [Dist]->[Dist]
prodPass []=[]
prodPass ((Dist f st (Ex q  m n a)):xs) | a=="pass" =(Dist f st (Ex q  m n a)):prodPass xs 
                                        | otherwise=prodPass xs
-----------produce a list of Dist where label(pass or fail) of Ex of Dist is only pass
prodFail:: [Dist]->[Dist]                                       
prodFail []=[]
prodFail ((Dist f st (Ex q  m n a)):xs) | a=="fail" =(Dist f st (Ex q  m n a)):prodFail xs 
                                        | otherwise=prodFail xs

mode :: (Num a, Ord a) => (b -> c -> Dist) -> a -> [c] -> b -> [Dist]
mode f num extlist nstudent | length(prodPass (closest f num extlist nstudent))>=length(prodFail (closest f num extlist nstudent))=prodPass (closest f num extlist nstudent)
                            | otherwise=prodFail (closest f num extlist nstudent)

label_of :: [Dist] -> ([Char],[Char])
label_of ((Dist f (NewSt _ _ name) (Ex _  _ _ label)):xs)=(name,label)

classify :: (Ord a, Num a) => (b -> c -> Dist) -> a -> [c] -> b -> ([Char],[Char])
classify f num extlist nstudent=label_of (mode f num extlist nstudent)

classify_all :: (Num a, Ord a) => (b -> c -> Dist) -> a -> [c] -> [b] -> [([Char],[Char])]
classify_all f num extlist []=[]
classify_all f num extlist (x:xs)=(classify f num extlist x):(classify_all f num extlist xs)	









					