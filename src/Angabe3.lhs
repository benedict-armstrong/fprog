> module Angabe3 where


1. Vervollstaendigen Sie gemaess Angabentext!
2. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
3. Achten Sie darauf, dass Gruppe Leserechte fuer Ihre Abgabedatei hat!

\begin{code}

type UntereSchranke = Int
type ObereSchranke  = Int
data Intervall  = IV (UntereSchranke,ObereSchranke)
                    | Leer
                    | Ungueltig


\end{code}

Aufgabe A.1s

\begin{code}

instance Show Intervall where
    show Leer = "<>"
    show Ungueltig = "Kein Intervall"
    show (IV (a, b))
            | a < b = "<" ++ show a ++ "," ++ show b ++ ">"
            | otherwise = show Leer


\end{code}

Die Instanzdeklaration fuer Show geht folgendermassen vor: ... 

Aufgabe A.2

\begin{code}

instance Eq Intervall where
    IV (a,b) == IV (c,d) = a==c && b == d
    _ == Leer = True
    Leer == _ = True
    _ == _ = error "Vergleich nicht moeglich"

\end{code}

Die Instanzdeklaration fuer Eq geht folgendermassen vor: ... 


Aufgabe A.3

\begin{code}

instance Ord Intervall where
    Ungueltig <= _ = error "Vergleich nicht moeglich"
    _ <= Ungueltig = error "Vergleich nicht moeglich"
    Leer <= _ = True
    _ <= Leer = False
    (IV (a,b)) <= (IV(c,d)) = a >= c && b <= d
    


\end{code}

Die Instanzdeklaration fuer  Ord geht folgendermassen vor: ... 


Aufgabe A.4

\begin{code}

instance Num Intervall where
    Ungueltig + _ = error "Vergleich nicht moeglich"
    _ + Ungueltig = error "Vergleich nicht moeglich"
    IV (a,b) + IV (c,d) = IV ((a+c), (b+d))
    Ungueltig - _ = error "Vergleich nicht moeglich"
    _ - Ungueltig = error "Vergleich nicht moeglich"
    IV (a,b) - IV (c,d) = IV ((a-c), (b-d))
    abs Ungueltig = error "Vergleich nicht moeglich"
    abs (IV (a,b))
            | a > b = Leer
            | (a < 0 && 0 <= b) = IV (0, b)
            | a < 0 && b < 0 = IV (-a,-b)
            | otherwise = IV (a,b)
    



\end{code}

Die Instanzdeklaration fuer Num geht folgendermassen vor: ... 


Aufgabe A.5


\begin{code}

instance Enum Intervall where
     toEnum m = IV (m,m)
     fromEnum Ungueltig = error "Operation nicht moeglich"
     fromEnum (IV (a,b))
                | a == b = a
                | otherwise = error "Operation nicht moeglich"



\end{code}

Die Instanzdeklaration fuer Enum geht folgendermassen vor: ... 


Aufgabe A.6

\begin{code}

class Kanonisch a where
    kanonisch :: a -> a

instance Kanonisch Intervall where
    kanonisch Ungueltig = Ungueltig
    kanonisch Leer = Leer
    kanonisch (IV (a,b))
            | a > b = Leer
            | otherwise = IV (a, b)


\end{code}

Die Instanzdeklaration fuer Kanonisch geht folgendermassen vor: ... 


Aufgabe A.7

\begin{code}

class Element a where
    is_elem :: Int -> a -> Maybe Bool

instance Element Intervall where
    is_elem _ Ungueltig = Nothing
    is_elem x (IV (a,b)) = Just (x > a && x < b)

\end{code}

Die Instanzdeklaration fuer Element geht folgendermassen vor: ... 


Aufgabe A.8


class (Element a) => Code a where
    codiere :: [Int] -> a
    decodiere :: a -> Maybe [Int]

instance Code Intervall where
    codiere (f:xs)
                | f <= (last xs) = (IV (f, (last xs)))
                | otherwise = Ungueltig



Die Instanzdeklaration fuer Code geht folgendermassen vor: ... 
isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (f:s:xs) = f < s && isSorted xs
% decodiere Leer = Just []
    % decodiere Ungueltig = Nothing
    % decodiere (IV (a,b)) = Just [a..b]
% Aufgabe A.9

% > class (Ord a,Enum a) => ExTest a where
% > extrahiere  :: Maybe [a] -> [a]
% > ist_aufsteigend :: [a] -> Bool
% > ist_lueckenlos :: [a] -> Bool
% > ist_laL_Element :: a -> Maybe [a] -> Bool
% > extrahiere a = a
% > ist_aufsteigend a = a
% > ist_lueckenlos a = a
% > ist_laL_Element a b = a

% Die Instanzdeklaration fuer ExTest geht folgendermassen vor: ... 


