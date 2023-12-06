module BinaryNumber where

import Data.List
import Data.Tuple (swap)
import Data.List (unfoldr)
import Data.List (mapAccumL)

{-
    Reprezentarea unui număr binar ca listă finită sau infinită de biți.
    Primul bit este cel mai puțin semnificativ.

    De exemplu, 6 poate fi reprezentat ca [0, 1, 1, 0, 0, ...].

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type BinaryNumber = [Int]

{-
    *** TODO ***

    Transformă un număr din reprezentarea binară finită în cea zecimală.
    
    Constrângeri: funcția trebuie definită
    * în stil point-free (fără explicitarea parametrului formal)
    * utilizând o funcțională (fără recursivitate explicită).

    Exemple:
    
    > toDecimal [0,1,1]
    6
-}
toDecimal :: BinaryNumber -> Int
toDecimal = fst . foldl' (\(acc, p) x -> (acc + p * x, 2 * p)) (0, 1)
{-
ia prima chestie din perchea (0, 1) unde 0 e acc (accumulatorul, suma) iar 1 e un p, o putere
caci in pseudocod vedeam asta asa
p = 1 -> puteri de 2, 2^0
cat timp lista nu e vida
acc = acc + head (lista) * p
p = p * 2
lista = tail (lista)
-}


{-
    *** TODO ***

    Transformă un număr din reprezentarea zecimală în cea binară infinită.

    Constrângeri: pt bonus 10p, funcția trebuie definită
    * în stil point-free (fără explicitarea parametrului formal)
    * utilizând funcționala unfoldr (fără recursivitate explicită).

    Spre deosebire de foldr, care împăturește o listă la o singură valoare,
    unfoldr desfășoară procesul invers, construind o listă pe baza unei valori
    inițiale.
    
    Hint: divMod.

    Exemple:
    
    > take 10 $ toBinary 6
    [0,1,1,0,0,0,0,0,0,0]
-}
toBinary :: Int -> BinaryNumber
toBinary = unfoldr (\n -> Just (snd (divMod n 2), div n 2))
{- 
am incercat sa ma tin de constrangeri
Deci: unfoldl vrea ca tip (Int, Maybe). Maybe e in lab 3 de haskell (7 cred ca e).
divMod intoarce o pereche (Int, Int) din care prima chestie e impartitul si a doua e restul
ca sa fie infinit trebuie fara verificare n == 0
initial luam un unfoldr (\n -> if n == 0 then [] else [snd (divMod n 2)] ++ toBinary (div n 2))
apoi, pe baza arg ptr unfoldl am modificat la (\n -> if n == 0 then Nothing else Just snd (divMod n 2))
apoi, la varianta finala
-}


{-
    *** TODO ***

    Incrementează cu 1 reprezentarea finită sau infinită a unui număr binar.
    Atenție la transport!

    Constrângeri: utilizați
    * recursivitate explicită
    * pattern matching.

    Exemple:

    > inc [0,1,1] 
    [1,1,1]

    > inc [1,1,1]
    [0,0,0,1]
-}

inc :: BinaryNumber -> BinaryNumber
inc bits
  | allOnes bits = 1 : makeZeros bits
  | otherwise = inc' bits
  where
    allOnes :: BinaryNumber -> Bool
    allOnes [] = False
    allOnes [1] = True
    allOnes (1:xs) = allOnes xs
    allOnes _ = False

    makeZeros :: BinaryNumber -> BinaryNumber
    makeZeros bits = 0 : zeros (length bits - 1)
      where
        zeros 0 = []
        zeros n = 0 : zeros (n - 1)

    inc' :: BinaryNumber -> BinaryNumber
    inc' [] = [1]
    inc' (0:xs) = 1 : xs
    inc' (1:xs) = 0 : inc' xs
       
        
{-
   *** TODO ***

    Decrementează cu 1 reprezentarea finită sau infinită a unui număr binar.
    Atenție la împrumut!

    Constrângeri: utilizați
    * recursivitate explicită
    * pattern matching.

    Exemple:

    > dec [1,1,1]
    [0,1,1]

    > dec [0,0,0,1]
    [1,1,1]
-}
dec :: BinaryNumber -> BinaryNumber
dec bits
  | allZeros bits = makeOnes bits
  | otherwise = dec' bits
  where
    allZeros :: BinaryNumber -> Bool
    allZeros [] = False
    allZeros [0] = True
    allZeros (0:xs) = allZeros xs
    allZeros _ = False

    makeOnes :: BinaryNumber -> BinaryNumber
    makeOnes [] = [1]
    makeOnes (0:xs) = 1 : xs
    makeOnes (1:xs) = 0 : makeOnes xs

    dec' :: BinaryNumber -> BinaryNumber
    dec' [] = []
    dec' [0] = [0]
    dec' [1] = [0]
    dec' (0:xs) = 1 : dec' xs
    dec' (1:xs) = 0 : xs


{-
    *** TODO ***

    Adună două numere binare, asumând reprezentări infinite, pentru ușurința
    aplicării funcționalelor.

    Constrângeri: utilizați
    * where sau let
    * pt bonus 10p, funcționala mapAccumL (fără recursivitate explicită).

    mapAccumL are tipul (a -> b -> (a, c)) -> a -> [b] -> (a, [c]).
    Așa cum sugerează numele, combină comportamentele funcționalelor:
    * map, transformând element cu element [b] în [c]
    * foldl, utilizând în același timp un acumulator de tipul a.

    Exemple:

    > take 10 $ add (toBinary 74) (toBinary 123)
    [1,0,1,0,0,0,1,1,0,0]
    
    > toDecimal $ take 10 $ add (toBinary 74) (toBinary 123)
    197
-}

add :: BinaryNumber -> BinaryNumber -> BinaryNumber
add bits1 bits2 = snd $ mapAccumL (\c (a, b) -> let s = a + b + c
                                       in (s `div` 2 , s `mod` 2)) 0 (zipLongest bits1 bits2)
    where
      zipLongest xs ys = zip (pad xs) (pad ys)
      pad xs = xs ++ repeat 0

{-
    *** TODO ***

    În pregătirea operației de înmulțire a două numere binare, cu reprezentări
    infinite, stivuiește copii deplasate la dreapta ale lui bits1, înmulțite
    cu bit-ul curent din bits2. Deplasarea se face adăugând la stânga lui bits1
    un număr de 0-uri dat de numărul liniei curente. Întoarce o listă infinită
    de liste infinite.

    Vizual:

    0 1 1 0 ... *   <- bits1
    1 0 1 0 ...     <- bits2
    -----------
   |0 1 1 0 ...        înmulțire bits1 cu 1 și deplasare 0 poziții la dreapta
    0|0 0 0 0 ...      înmulțire bits1 cu 0 și deplasare 1 poziție la dreapta
    0 0|0 1 1 0 ...    înmulțire bits1 cu 1 și deplasare 2 poziții la dreapta

    Constrângeri:
    * Corpul funcției trebuie să fie un list comprehension.
    * Nu utilizați recursivitate explicită.

    Hint: iterate pt generarea secvențelor de deplasare spre dreapta cu biți 0.

    Exemple:

    (exemplul vizual)
    > take 3 $ map (take 6) $ stack (toBinary 6) (toBinary 5)
    [[0,1,1,0,0,0],[0,0,0,0,0,0],[0,0,0,1,1,0]]
-}

zeros :: [Int]
zeros = 0 : zeros

stack :: BinaryNumber -> BinaryNumber -> [BinaryNumber]
stack bits1 bits2 = [foldr (++) [] [take i zeros, if b == 1 then bits1 else zeros] | (i, b) <- zip [0..] bits2]
    where zeros = 0 : zeros

{-
    *** TODO ***

    Întoarce o listă infinită de numere binare, care pe poziția i >= 0 conține
    rezultatul înmulțirii lui bits1 cu numărul format din primii i biți
    ai lui bits2, i.e. suma primelor i linii întoarse de stack.

    Constrângeri:
    * Utilizați funcționala scanl (fără recursivitate explicită).

    Spre deosebire de foldl, care întoarce acumulatorul final, scanl întoarce
    o listă cu toate acumulatoarele intermediare.

    Exemple:
    
    > take 5 $ map (toDecimal . take 10) $ multiply (toBinary 6) (toBinary 5) 
    [0,6,6,30,30]
-}

multiply :: BinaryNumber -> BinaryNumber -> [BinaryNumber]
multiply bits1 bits2 = scanl addBinaryNumbers zeros (stack bits1 bits2)
    where 
        addBinaryNumbers acc curr = zipWith (\a b -> (a + b) `mod` 2) acc curr

{-
    *** TODO ***

    Întrebare de reflecție, la care veți răspunde la prezentarea temei.

    Având în vedere că liniile întoarse de stack care conțin exclusiv biți 0
    nu contribuie la rezultatul unei înmulțiri, să presupunem că modificăm
    definiția funcției stack astfel încât să construiască doar liniile utile;
    de exemplu, folosind filter sau pattern matching în list comprehension
    pt a păstra doar biții 1 din bits2. Ce probleme ar putea crea această
    abordare?
-}
