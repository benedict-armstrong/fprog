> module Angabe4 where

1. Vervollstaendigen Sie gemaess Angabentext!
2. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm!
3. Achten Sie darauf, dass Gruppe Leserechte fuer Ihre Abgabedatei hat!
 

Aufgabe A.1

\begin{code}

type Nat0   = Int
type Ebene  = Nat0
type Breite = Nat0
type Text   = String
data BBaum = Blatt Text
            | Knoten Text BBaum BBaum deriving (Eq,Show)
data Auswertung = Ausw Breite [Ebene] deriving (Eq,Show)

breitest :: BBaum -> Auswertung
breitest (Blatt _) = Ausw 1 [0]
breitest b = (Ausw (maximum width) (apperancesInList width (maximum width)))
            where width = map (countAt b) [1..(height b)]

apperancesInList :: [Int] -> Int -> [Int]
apperancesInList xs e = 
            let aIL :: [Int] -> Int -> Int -> [Int]
                aIL [] _ _ = []
                aIL (x:xs) e i
                        | x == e = i : (aIL xs e (i+1))
                        | x /= e = (aIL xs e (i+1))
            in aIL xs e 0

height :: BBaum -> Int
height (Blatt _) = 1
height (Knoten _ k1 k2) = 1 + max (height k1) (height k2)

countAt :: BBaum -> Int -> Int
countAt _ 0 = 0
countAt _ 1 = 1
countAt (Blatt _) _ = 0
countAt (Knoten _ k1 k2) n = (countAt k1 (n-1)) + (countAt k2 (n-1))


addWidth :: Auswertung -> Int -> Auswertung
addWidth (Ausw b xs) n = (Ausw (b + n) xs)


bfs :: [BBaum] -> [Text]
bfs [] = []
bfs ((Blatt text):xs) = [text] ++ bfs xs
bfs ((Knoten text k1 k2):xs) = [text] ++ (bfs (xs ++ [k1] ++ [k2]))


\end{code}
breitest geht folgendermassen vor: ...

Aufgabe A.2

\begin{code}

tae :: BBaum -> Ebene -> Maybe [Text]
tae b e 
        | e < height b = Just (test b e)
        | otherwise = Nothing

test :: BBaum -> Ebene -> [Text]
test (Blatt text) 0 = [text]
test (Knoten text _ _) 0 = [text]
test (Blatt _) _ = []
test (Knoten _ k1 k2) i = (test k1 (i-1)) ++ (test k2 (i-1))

\end{code}

tae geht folgendermassen vor: ...

Aufgabe A.3

\begin{code}

data TBaum    = TB
                | TK TBaum TBaum TBaum deriving (Eq,Show)
data Richtung = L | M | R deriving (Eq,Show)
type Weg      = [Richtung]
data TBaum'   = TB' Weg
                | TK' Weg TBaum' TBaum' TBaum' deriving (Eq,Show)

awi :: TBaum -> TBaum'
awi t = 
    let awi' :: TBaum -> Weg -> TBaum'
        awi' (TB) w = TB' w
        awi' (TK l m r) w = TK' w (awi' l (w++[L])) (awi' m (w++[M])) (awi' r (w++[R]))
    in awi' t []

\end{code}

awi geht folgendermassen vor: ...


Aufgabe A.4

\begin{code}

type Info a = [a]
data Baum a = B (Info a)
              | K (Baum a) (Info a) (Baum a)

instance Show a => Show (Baum a) where
    show (B xs) = "<" ++ show xs ++ ">"
    show (K b1 xs b2) = "<Wurzel " ++ show xs ++ " " ++ show b1 ++ " " ++ show b2 ++ ">"

instance Eq a => Eq (Baum a) where
    (B _) == (K _ _ _) = False
    (K _ _ _) == (B _) = False
    (B c1) == (B c2) = c1 == c2
    (K b1 c1 b2) == (K b3 c2 b4) = (c1 == c2) && (b1 == b3) && (b2 == b4)

instance Ord a => Ord (Baum a) where
    (B c1) < (K _ c2 _) = c1 `smallerThan` c2
    (K b1 c1 b2) < (K b3 c2 b4) = c1 `smallerThan` c2 && b1 < b3 && b2 < b4
    b1 <= b2 = b1 < b2 || b1 == b2


smallerThan :: Ord a => [a] -> [a] -> Bool
smallerThan _ [] = False
smallerThan [] _ = True
smallerThan (x:xs) (y:ys) = x < y && (xs `smallerThan` ys)


\end{code}

Die Instanzdeklarationen gehen folgendermassen vor: ...


Aufgabe A.5

\begin{code}

type UntereSchranke = Int
type ObereSchranke  = Int
data Intervall      = IV (UntereSchranke,ObereSchranke)
                      | Leer
                      | Ungueltig

instance Show Intervall where
    

\end{code}

