module BinomialHeap where

import Data.Function (on)
import Data.List
import Data.Maybe (fromMaybe)
import Data.List (minimumBy)

{-
    Reprezentarea unui arbore binomial, având priorități de tipul p și chei
    de tipul k. Conform proprietății definitorii a acestor arbori, fiecare
    copil i dintre cei r copii ai unui nod de rang r, cu 1 <= i <= r, trebuie
    să aibă exact r-i copii la rândul său, adică r-1, r-2, ..., 1, 0 copii,
    exact în această ordine descrescătoare. Dacă rădăcina are r copii, atunci,
    în conjuncție cu proprietate anterioară, întregul arbore are 2^r noduri.
-}
data BinomialTree p k
    = EmptyTree
    | Node { prio :: p, key :: k, children :: [BinomialTree p k] }
    deriving (Show, Eq)

{-
    Reprezentarea unui heap binomial, ca listă de arbori binomiali ce respectă
    proprietatea de heap. Mai precis, considerăm că, în fiecare arbore binomial,
    rădăcina are cea mai mică prioritate; analog pt subarbori. Câmpul size
    desemnează numărul de elemente din heap (nu numărul de arbori). Mai precis,
    dimensiunea este egală cu suma dimensiunilor arborilor din listă. Cum
    dimensiunea unui arbore de rang r este 2^r, dimensiunea este o sumă
    de puteri ale lui 2, unde exponenții sunt dimensiunile câmpurilor children
    (rangurile) din rădăcinile arborilor nevizi.
-}
data BinomialHeap p k = BinomialHeap { size :: Int, trees :: [BinomialTree p k] }
    deriving (Show, Eq)

{-
    *** TODO ***

    Construiește recursiv un arbore binomial de rang r din doi arbori binomiali
    de rang r-1, atașând unul dintre arbori drept prim copil al rădăcinii
    celuilalt. Maniera în care se realizează atașarea trebuie să țină cont
    de faptul că cei doi arbori respectă proprietatea de heap, și că arborele
    rezultant trebuie de asemenea să o respecte. Astfel, arborele cu cheia mai
    mică din rădăcină trebuie să încorporeze arborele cu cheia mai mare.

    Atenție! Cei doi arbori primiți ca parametru au întotdeauna același rang,
    conform principiului de construcție. Funcția nu necesită parcurgeri
    recursive, putând opera direct la nivelul rădăcinilor.

    Constrângeri: utilizați gărzi.

    Hint: pt pattern matching, pot fi utile alias-urile (@).

    Exemple:

    > attach (Node 0 'a' []) (Node 1 'b' [])
    Node {prio = 0, key = 'a', children = [Node {prio = 1, key = 'b', children = []}]}

    > attach (Node 1 'b' []) (Node 0 'a' [])
    Node {prio = 0, key = 'a', children = [Node {prio = 1, key = 'b', children = []}]}
-}

attach :: Ord p => BinomialTree p k -> BinomialTree p k -> BinomialTree p k
attach EmptyTree t = t
attach t EmptyTree = t
attach tree1@(Node p1 k1 c1) tree2@(Node p2 k2 c2)
  | p1 < p2   = Node p1 k1 (Node p2 k2 c2:c1)
  | otherwise = Node p2 k2 (Node p1 k1 c1:c2)

  -- asemanator ca in laboratorul de tipuri de date, exercitiul cu BSTree

{-
    *** TODO ***

    Introduce un arbore binomial nevid într-o listă cu alți arbori binomiali,
    toți arborii respectând proprietatea de heap. Cum nu pot exista simultan
    în listă doi arbori binomiali cu același rang, la întâlnirea unui arbore
    cu același rang cu cel care se dorește introdus, este necesară atșarea
    unuia la celălalt, cu crearea unui transport.

    Operația o oglindește pe cea de incrementare a unui număr binar din etapa 1.
    Astfel, elementele EmptyTree sunt analoagele biților 0, iar elementele Node,
    biților 1. O diferență este că, în această etapă, biții 1 „nu mai arată toți
    la fel”, ci elementele Node au rangul dat de poziția în listă. Spre exemplu:
    * un element Node de pe poziția 0 din listă trebuie să aibă rangul 0
      (dimensiunea 2^0 = 1)
    * un element Node de pe poziția 1 din listă trebuie să aibă rangul 1
      (dimensiunea 2^1 = 2)
    * un element Node de pe poziția 2 din listă trebuie să aibă rangul 2
      (dimensiunea 2^2 = 4)
    etc.

    Gestiunea transportului apărut în incrementare corespunde operației attach.
    Modul în care va fi utilizată mai departe funcția insertTree garantează
    respectarea presupunerilor funcției attach, cum că arborii primiți ca
    parametru au întotdeauna același rang.

    Constrângeri: utilizați
    * construcția case
    * funcția attach.

    Exemple:

    > insertTree (Node 1 'a' []) []
    [Node {prio = 1, key = 'a', children = []}]

    > insertTree (Node 2 'b' []) $ insertTree (Node 1 'a' []) []
    [ EmptyTree
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]

    > insertTree (Node 3 'c' []) $ insertTree (Node 2 'b' []) $ insertTree (Node 1 'a' []) []
    [ Node {prio = 3, key = 'c', children = []}
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]
-}

insertTree :: Ord p => BinomialTree p k -> [BinomialTree p k] -> [BinomialTree p k]
insertTree tree [] = [tree] -- daca e lista vida pune arborele
insertTree tree (EmptyTree:trees) = tree : trees 
-- daca e arbore vid e ca si cum ai avea 0 deci adaugi 1, adaugi un arbore
insertTree tree (t:trees) =
  case attach tree t of
    EmptyTree -> []
    othertree -> EmptyTree : insertTree othertree trees
    -- altfel, e ca și cum ai pune 1 la primul bit care e tot 1 si ala se face 0, 
    -- adica EmptyTree, pe care il adaugi la Lista obtinuta recursiv cu insertTree aplicat pe un nou arbore 
    -- obtinut cu attach si restul listei de arbore.

{-
    *** TODO ***

    Heap-ul vid.
-}
emptyHeap :: BinomialHeap p k
emptyHeap = BinomialHeap{ size = 0, trees = [] } 
-- initializare nou heap cu dimensiunea 0 si fara arbori, nici macar arbore vid

{-
    *** TODO ***

    Introduce o cheie cu prioritatea aferentă într-un heap binomial.

    Constrângeri: utilizați funcția insertTree.

    Exemple:

    > insert 1 'a' emptyHeap
    BinomialHeap 
        { size = 1
        , trees = [Node {prio = 1, key = 'a', children = []}]
        }

    > insert 2 'b' $ insert 1 'a' emptyHeap
    BinomialHeap
        { size = 2
        , trees = [ EmptyTree
                  , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
                  ]
        }

    > insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    BinomialHeap 
        { size = 3
        , trees = [ Node {prio = 3, key = 'c', children = []}
                  , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
                  ]
        }
-}

getSize :: BinomialHeap size trees -> Int -- extrage dimensiunea unui heap
getSize (BinomialHeap size _) = size

insert :: Ord p => p -> k -> BinomialHeap p k -> BinomialHeap p k
insert prio key heap@(BinomialHeap size trees) = BinomialHeap {size =  getSize heap + 1,  trees = insertTree (Node prio key []) trees}
-- am folosit alias pentru a fi mai usor ca sa extrag campul
-- da, puteam pune direct size, dar asta mi-am dat seama dupa ce m-am chinuit putin mai incolo la exercitiul urmator
-- deci daca adauga un nou arbore la heap, logic ca heap ul isi mareste dimensiunea
-- iar la trees se adauga acel arbore
-- aici mi-a fost prea sila sa scriu fara alias-uri =(

{-
    *** TODO ***

    Dacă heap-ul nu este vid, întoarce perechea formată din prioritatea minimă
    și cheia aferentă; în caz contrar, întoarce Nothing. Cum toți arborii din
    listă respectă proprietatea de heap, este suficient să parcurgeți doar
    rădăcinile pt a o determina pe cea cu prioritate minimă, fără a fi necesară
    explorarea nivelurilor inferioare ale arborilor.

    Constrângeri: pt selectarea arborilor nevizi din listă (ignorând elementele
    EmptyTree), utilizați list comprehension cu pattern matching.

    Hint: pt determinarea elementului minim dintr-o listă pe baza valorii
    calculate de o funcție numită criteriu, utilizați o expresie de forma:
    minimumBy (compare `on` criteriu) lista.

    Exemple:

    > findMin emptyHeap
    Nothing

    > findMin $ insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    Just (1,'a')
-}

getPriority :: BinomialTree p k -> Maybe p
getPriority (Node p _ _) = Just p
getPriority EmptyTree = Nothing 
-- trebuie luate aici toate cazurile, trebuie functie care merge si pe chestii vide
-- ma gandeam ca o puteam folosi pentru extragerea prioritatii aici, la findMin, dar nu merge
-- oricum o folosesc mai incolo 

findMin :: Ord p => BinomialHeap p k -> Maybe (p, k)
findMin (BinomialHeap _ []) = Nothing
findMin (BinomialHeap _ arb) =
  let arbori = [(Node p k c) | (Node p k c) <- arb] -- list comprehensions cu pattern matching
  -- pattern matching ul e pe (Node p k c)
  -- fiind nevid inseamna ca are prioritate, cheie si fii
  -- list comprehensions e ca ia element de tip (Node p k c) cu proprietatea ca se gaseste in lista de arbori arb
      minarb = minimumBy (compare `on` prio) arbori -- daca ati zis ca minimumBy, de ce nu?
  in Just (prio minarb, key minarb) -- accesare campuri din structura de nod de arbore

{-
    Funcția zipExtend este similară funcției predefinite zip. Scopul ei este
    de a compensa limitarea funcției zip, care se oprește când atinge sfârșitul
    listei mai scurte. Primii doi parametri reprezintă valori cu care se extind
    prima, respectiv a doua listă, în funcție de care listă este mai scurtă.
    O puteți folosi în cele ce urmează.

    Exemple:

    > zipExtend 0 'z' [1,2] "abcd"
    [(1,'a'),(2,'b'),(0,'c'),(0,'d')]

    > zipExtend 0 'z' [1,2,3,4] "ab"
    [(1,'a'),(2,'b'),(3,'z'),(4,'z')]
-}
zipExtend :: a -> b -> [a] -> [b] -> [(a, b)]
zipExtend a' _  [] bs = zip (repeat a') bs
zipExtend _  b' as [] = zip as (repeat b')
zipExtend a' b' (a : as) (b : bs) = (a, b) : zipExtend a' b' as bs
-- asta e din scheletul temei -^\(0_0)/^-

{-
    *** TODO ***

    Combină două liste de arbori binomiali care respectă proprietatea de heap.
    Observațiile din comentariile funcției insertTree, legate de necesitatea
    atașării arborilor cu același rang, rămân valabile.

    Operația o oglindește pe cea de adunare a două numere binare din etapa 1.

    Constrângeri:
    * evitați recursivitatea explicită
    * utilizați funcția zipExtend pt a facilita aplicarea unor funcționale.

    Exemple:

    > mergeTrees [Node 1 'a' []] []
    [Node {prio = 1, key = 'a', children = []}]

    > mergeTrees [Node 1 'a' []] [Node 2 'b' []]
    [ EmptyTree
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]

    > mergeTrees [Node 3 'c' []] $ mergeTrees [Node 1 'a' []] [Node 2 'b' []]
    [ Node {prio = 3, key = 'c', children = []}
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]
-}

mergeTrees :: Ord p => [BinomialTree p k] -> [BinomialTree p k] -> [BinomialTree p k]
mergeTrees arbore1 arbore2 = map (uncurry attach) $ zipExtend EmptyTree EmptyTree arbore1 arbore2
-- folosit zipExtend V
-- ne-folosit recursivitate explicita V
-- map vrea mai multe argumente

{-
    *** TODO ***

    Combină două heap-uri binomiale.

    Constrângeri: utilizați funcția mergeTrees.

    Exemple: similare cu cele de la mergeTrees.
-}
merge :: Ord p => BinomialHeap p k -> BinomialHeap p k -> BinomialHeap p k
merge h1@(BinomialHeap size1 trees1) h2@(BinomialHeap size2 trees2) = 
    let combinati = foldr insertTree trees2 trees1
    in BinomialHeap (getSize h1 + getSize h2) combinati