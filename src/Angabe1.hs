module Angabe1 where

type Nat1 = Int

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. LÃ¶schen Sie keine Deklarationen aus diesem Rahmenprogramm!
   3. Achten Sie darauf, dass Gruppe Leserechte fuer Ihre Abgabedatei hat!
-}


-- Aufgabe A.1

filtere :: Nat1 -> [Int] -> [Int] 
filtere n list = reverse (removeDuplicates ( quickSort[x | x <- list, countInList list x == n]))

-- zählt wie oft ein Int n in einer liste vorkommt
countInList :: [Int] -> Int -> Int
countInList list n = length (filter (==n) list)

-- entfernt werte die mehrmals vorkommen (nimmt den letzten)
removeDuplicates :: [Int] -> [Int]
removeDuplicates [] = []
removeDuplicates (x:list) =
   if x `elem` list then removeDuplicates list 
   else x : removeDuplicates list

-- Aufgabe A.2

kommt_vor :: Int -> [(Int,Int)] -> Bool
kommt_vor n list = 
      if  n `elem` [x | (x,y) <- list] then True
      else if  n `elem` [y | (x,y) <- list] then True
      else False

-- Aufgabe A.3

aus :: [Int] -> [Int]
aus [] = []
aus list = concatMap (replicate (maxCountInList list)) (removeDuplicates (quickSort list))

-- liefert die anzahl des am häufigsten vorkommenden Elements einer liste
maxCountInList :: [Int] -> Int
maxCountInList list = maximum (map (countInList list) list)

--sortiert eine liste aufsteigend
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (p:list) = (quickSort (filter (< p) list)) ++ [p] ++ (quickSort (filter (>= p) list))

-- Aufgabe A.4

h :: String -> String -> Int
h s1 s2 =
   if (length s1) /= (length s2) then -1
   else sum (map compareTuple (zip s1 s2))

-- vergleicht ein tupel (paar) =0 wenn gleich =1 wenn verschieden
compareTuple :: (Char,Char) -> Int
compareTuple t = 
      if fst t == snd t then 0
      else 1