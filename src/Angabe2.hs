module Angabe2 where


{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. LÃ¶schen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   3. Achten Sie darauf, dass Gruppe Leserechte fuer Ihre Abgabedatei hat!
-}


type Nat0 = Integer


-- Aufgabe A.1

data IstEinsBinaerPrim = Ja | Nein deriving (Eq,Show)

ist_einsbp :: Nat0 -> IstEinsBinaerPrim
ist_einsbp n
         | isPrime (ones (toBinary n)) = Ja
         | otherwise = Nein

toBinary :: Nat0 -> [Integer]
toBinary 0 = [0]
toBinary n = rest  ++ [(n `mod` 2)]
   where rest
            | n `div` 2 /= 0 = toBinary (n `div` 2)
            | otherwise = []

ones :: [Integer] -> Int
ones [] = 0
ones list
      | head list == 1 = 1 + ones (tail list)
      | otherwise = ones (tail list)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n
      | even n && n /= 2 = False
      | otherwise = null [x | x <- [2..(n-1)], n `mod` x == 0]



{- ist_einsbp geht folgendermassen vor: ... 
-}


-- Aufgabe A.2

type Von           = Nat0
type Bis           = Nat0
type VonBis        = (Von,Bis)
type Anzahl0bps    = Int
type Anzahl1bps    = Int
type Anzahlen01bps = (Anzahl0bps,Anzahl1bps)

anz01bps :: VonBis -> Anzahlen01bps
anz01bps x
      | fst x <= snd x = (ist_nullbp_anzahl [(fst x) .. (snd x)], ist_einsbp_anzahl [(fst x) .. (snd x)])
      | otherwise = (-1,-1)

ist_einsbp_anzahl :: [Nat0] -> Int
ist_einsbp_anzahl [] = 0
ist_einsbp_anzahl (x:xs)
            | (ist_einsbp x) == Ja = 1 + ist_einsbp_anzahl xs
            | otherwise = ist_einsbp_anzahl xs

ist_nullbp_anzahl :: [Nat0] -> Int
ist_nullbp_anzahl [] = 0
ist_nullbp_anzahl (x:xs)
            | (ist_nullbp x) == Ja = 1 + ist_nullbp_anzahl xs
            | otherwise = ist_nullbp_anzahl xs

ist_nullbp :: Nat0 -> IstEinsBinaerPrim
ist_nullbp n
         | isPrime (zeros (toBinary n)) = Ja
         | otherwise = Nein

zeros :: [Integer] -> Int
zeros [] = 0
zeros list
      | head list == 0 = 1 + zeros (tail list)
      | otherwise = zeros (tail list)


{- anz01bps geht folgendermassen vor: ... 
-}


-- Aufgabe A.3

type Wort      = String
type Wortliste = [Wort]

liefere_woerter_in :: String -> Wortliste
liefere_woerter_in = lwi

lwi :: String -> Wortliste
lwi [] = [""]
-- lwi (c:string)
--    | c == ' ' || c == '\n' || c == '\t' = lwi string
--    | otherwise = (c : head (lwi string)) : tail (lwi string)

{- lwi geht folgendermassen vor: ... 
-}


-- Aufgabe A.4

type Hammingabstand = Int

hM :: [String] -> Hammingabstand
hM [] = -1
hM list
      | not (equalLen list) = -1
      | otherwise = hamming list

hamming :: [String] -> Hammingabstand
--hamming list = minimum (scanl (\acc x -> minimum (map (h x) list) ) 10000 list 
hamming (s:list)
         | null list = 100000
         | otherwise = minimum (minimum ( map (h s) list ) : [hamming list])


equalLen :: [String] -> Bool
equalLen list = if (foldl (\ acc x -> if acc == (length x) then acc else -1) (length (head list)) list) == -1 then False else True

h :: String -> String -> Int
h s1 s2 =
   if (length s1) /= (length s2) then -1
   else sum (map compareTuple (zip s1 s2))

-- vergleicht ein tupel (paar) =0 wenn gleich =1 wenn verschieden
compareTuple :: (Char,Char) -> Int
compareTuple t = 
      if fst t == snd t then 0
      else 1



{- hM geht folgendermassen vor: ... 
-}
