module Angabe5 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. LÃ¶schen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   3. Achten Sie darauf, dass Gruppe Leserechte fuer Ihre Abgabedatei hat!
-}

type Nat0 = Int

type Nat1 = Int

type Vorname = String

type Nachname = String

data Partei = ABC | DEF | GHI | JKL | MNO deriving (Eq, Show)

data Wahlwerber = WW Vorname Nachname Partei deriving (Eq, Show)

type Wahlvorschlag = [Wahlwerber]

type Wahlvorschlagsplatz = Nat1

type Wahlsieger = (Wahlwerber, Wahlvorschlagsplatz)

type Stimmzettel = [Wahlvorschlagsplatz]

type Wahl = [Stimmzettel]

type Gueltig = [Stimmzettel]

type Ungueltig = [Stimmzettel]

type Platz_1_Stimmen = Nat0

data Wahlausgang
  = Ungueltiger_Wahlvorschlag
  | Keine_gueltigen_Stimmen
  | Gewaehlt_ist Wahlwerber
  | Kein_Wahlsieger_Wahlwiederholung
  deriving (Eq, Show)

data Groesste_Wahlverlierer
  = GWV [Partei]
  | Keine
  | Analyse_nicht_moeglich
  deriving (Eq, Show)

-- Aufgabe A.1

ist_gueltiger_Wahlvorschlag :: Wahlvorschlag -> Bool
ist_gueltiger_Wahlvorschlag [] = False
ist_gueltiger_Wahlvorschlag _ = True

{- ist_gueltiger_Wahlvorschlag geht folgendermassen vor: ...
-}

-- Aufgabe A.2

ist_gueltiger_Stimmzettel :: Wahlvorschlag -> Stimmzettel -> Bool
ist_gueltiger_Stimmzettel wv [] = ist_gueltiger_Wahlvorschlag wv
ist_gueltiger_Stimmzettel wv st = (ist_gueltiger_Wahlvorschlag wv) && (length st) <= (length wv) && everyValueSmallerOrEqual st (length wv) && unique st

everyValueSmallerOrEqual :: (Ord a) => [a] -> a -> Bool
everyValueSmallerOrEqual [] _ = True
everyValueSmallerOrEqual (x : xs) n = x <= n && (everyValueSmallerOrEqual xs n)

unique :: (Eq a) => [a] -> Bool
unique [] = True
unique (x : xs) = not (x `elem` xs) && unique xs

{- ist_gueltiger_Stimmzettel geht folgendermassen vor: ...
-}

-- Aufgabe A.3

trenne_Stimmzettel :: Wahlvorschlag -> Wahl -> (Gueltig, Ungueltig)
trenne_Stimmzettel _ [] = ([], [])
trenne_Stimmzettel wv (st : wa)
  | ist_gueltiger_Stimmzettel wv st = let getrennt = trenne_Stimmzettel wv wa in ([st] ++ fst getrennt, snd getrennt)
  | otherwise = let getrennt = trenne_Stimmzettel wv wa in (fst getrennt, [st] ++ snd getrennt)

{- trenne_Stimmzettel geht folgendermassen vor: ...
-}

-- Aufgabe A.4

auszaehlen :: Wahlvorschlag -> Wahl -> Maybe [Platz_1_Stimmen]
auszaehlen _ [] = Just []
auszaehlen wv votes
  | alle_Stimmzettel_gueltig wv votes = auszaehlen' wv votes
  | otherwise = Nothing

auszaehlen' :: Wahlvorschlag -> Wahl -> Maybe [Platz_1_Stimmen]
auszaehlen' wv votes = Just (map (count (firsts votes)) [1 .. (length wv)])

firsts :: [[Int]] -> [Int]
firsts [] = []
firsts (x : xs)
  | x /= [] = [head x] ++ firsts xs
  | otherwise = [] ++ firsts xs

count :: (Eq a) => [a] -> a -> Int
count [] _ = 0
count (x : xs) n
  | x == n = 1 + count xs n
  | otherwise = count xs n

alle_Stimmzettel_gueltig :: Wahlvorschlag -> [Stimmzettel] -> Bool
alle_Stimmzettel_gueltig _ [] = True
alle_Stimmzettel_gueltig wv (vote : votes) = ist_gueltiger_Stimmzettel wv vote && alle_Stimmzettel_gueltig wv votes

{- auszaehlen geht folgendermassen vor: ...
-}

-- Aufgabe A.5

wahlsieger :: Wahlvorschlag -> Maybe [Platz_1_Stimmen] -> Maybe Wahlsieger
wahlsieger _ Nothing = Nothing
wahlsieger wv (Just votes)
  | not (ist_gueltiger_Wahlvorschlag wv) = Nothing
  | length wv /= length votes = Nothing
  | not (majority votes) = Nothing
  | otherwise = Just (maximumTupel (zip wv votes))

majority :: (Ord a, Num a) => [a] -> Bool
majority xs = (sum xs) < (maximum xs) * 2

maximumTupel :: Ord a => [(t, a)] -> (t, a)
maximumTupel (x : xs) = maxSnd x xs
  where
    maxSnd currentMax [] = currentMax
    maxSnd (f, s) (x : xs)
      | s < (snd x) = maxSnd x xs
      | otherwise = maxSnd (f, s) xs

{- wahlsieger geht folgendermassen vor: ...
-}

-- Aufgabe A.6

ausscheiden :: Wahl -> [Platz_1_Stimmen] -> Wahl
ausscheiden [] _ = []
ausscheiden (vote : votes) total = [throwOut vote (snd (minimum (zip total [1, 2 ..])))] ++ ausscheiden votes total

throwOut :: (Eq a) => [a] -> a -> [a]
throwOut [] _ = []
throwOut (x : xs) n
  | x == n = throwOut xs n
  | otherwise = [x] ++ throwOut xs n

{- ausscheiden geht folgendermassen vor: ...
-}

-- Aufgabe A.7

wahlausgang :: Wahlvorschlag -> Wahl -> Wahlausgang
wahlausgang [] _ = Ungueltiger_Wahlvorschlag
wahlausgang _ [] = Kein_Wahlsieger_Wahlwiederholung
wahlausgang wv votes
  | not (ist_gueltiger_Wahlvorschlag wv) = Ungueltiger_Wahlvorschlag
  | noWinner wv votes = Kein_Wahlsieger_Wahlwiederholung
  | otherwise =
    let (Just p1s) = auszaehlen wv (fst (trenne_Stimmzettel wv votes))
        sieger = wahlsieger wv (Just p1s)
     in if sieger == Nothing
          then wahlausgang wv (ausscheiden votes p1s)
          else calcWinner sieger

calcWinner :: Maybe Wahlsieger -> Wahlausgang
calcWinner Nothing = Kein_Wahlsieger_Wahlwiederholung
calcWinner (Just w) = Gewaehlt_ist (fst w)

noWinner :: Wahlvorschlag -> Wahl -> Bool
noWinner wv votes = let list = (removeZeros (auszaehlen wv votes)) in all (== head list) list && length list > 1

removeZeros :: Maybe [Int] -> [Int]
removeZeros Nothing = []
removeZeros (Just []) = []
removeZeros (Just (x : xs))
  | x == 0 = removeZeros (Just xs)
  | otherwise = [x] ++ removeZeros (Just xs)

{- wahlausgang eht folgendermassen vor: ...
-}

-- -- Aufgabe A.8

-- wahlanalyse :: Wahlvorschlag -> Wahl -> Groesste_Wahlverlierer

-- {- wahlanalyse geht folgendermassen vor: ...
-- -}
