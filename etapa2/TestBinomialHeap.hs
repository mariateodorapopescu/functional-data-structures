module TestBinomialHeap where

import Data.List (sort)
import Data.Maybe (Maybe)

import BinomialHeap
import TestPP

{-
    Construiește un arbore binomial de rang dat, începând cu prioritatea și cu cheia
    date.
-}
dummyTreeOfRank :: (Num p, Num k) => Int -> p -> k -> BinomialTree p k
dummyTreeOfRank 0 p k = Node p k []
dummyTreeOfRank rank startPrio startKey = top{children=bot : children top}
    where
        top = dummyTreeOfRank (rank-1) startPrio startKey
        bot = dummyTreeOfRank (rank-1) (startPrio+1) (startKey + 2^(rank-1))

{-
    Construiește o listă de arbori binomiali de ranguri date.
    Sunt permise rangurile negative, iar acestea dau naștere unor arbori vizi.
-}
dummyTrees :: (Num p, Num k) => [Int] -> [BinomialTree p k]
dummyTrees = map buildTree
    where
        buildTree rank
            | rank < 0 = EmptyTree
            | otherwise = dummyTreeOfRank rank 0 0

{-
    Construiește un heap binomial, folosindu-se de funcția anterioară.
-}
dummyHeap :: (Num p, Num k) => [Int] -> BinomialHeap p k
dummyHeap lst = BinomialHeap size trees
    where
        trees = dummyTrees lst
        size = sum (map getTreeSize trees)

{-
    Verifică proprietatea de arbore binomial care respectă suplimentar
    proprietatea de heap.
-}
treeProperty :: Ord p => BinomialTree p k -> Bool
treeProperty EmptyTree = True
treeProperty tree =
    let
        noChildren = length $ children tree
        condNoChildren = f (children tree) (noChildren - 1)
            where
                f [] (-1) = True
                f (tree : trees) n = (length (children tree) == n) && f trees (n - 1)
                f _ _ = False
        condHeap = null (children tree) || prio tree <= minimum (map prio (children tree))
    in
        condHeap && foldl (&&) condNoChildren (map treeProperty (children tree))

{-
    Verifică proprietatea de heap binomial: arborii sunt testați cu funcția
    de mai sus, iar dimensiunea heap-ului trebuie să coincidă cu suma
    dimensiunilor arborilor.
-}
binomialHeapProperty :: (Ord p, Eq k) => BinomialHeap p k -> Bool
binomialHeapProperty (BinomialHeap reportedSize list) = sizeOk && treesOk
    where
        sizeOk = reportedSize == realSize && theoreticalSize == realSize
        realSize = sum $ map getTreeSize list
        theoreticalSize = sum $ map ((2^) . length . children) $ filter (/= EmptyTree) list

        treesOk = treesIndividiually && treesPositioned
        treesIndividiually = all treeProperty list
        treesPositioned =
            all    (\(r, t) -> r == (length . children) t) .
            filter (\(_, t) -> t /= EmptyTree) .
            zip [0..] $ list

        rank = length list

{-
    Calculează dimensiunea unui arbore binomial, fără a presupune corectitudinea
    construcției acestuia.
-}
getTreeSize :: BinomialTree p k -> Int
getTreeSize EmptyTree = 0
getTreeSize (Node prio key children) = 1 + sum (map getTreeSize children)

{-
    Verifică dacă o listă de arbori binomiali respectă proprietățile de heap binomial.
-}
checkResult :: (Ord p, Eq k) => [BinomialTree p k] -> Bool
checkResult trees = binomialHeapProperty (BinomialHeap (sum (map getTreeSize trees)) trees)

{-
    Extrage perechile (prioritate, cheie) prezente într-un arbore.
-}
flattenTree :: BinomialTree p k -> [(p, k)]
flattenTree EmptyTree = []
flattenTree (Node p k c) = (p, k) : concatMap flattenTree c

{-
    Extrage perechile (prioritate, cheie) prezente într-un heap.
-}
flattenHeap :: BinomialHeap p k -> [(p, k)]
flattenHeap BinomialHeap{trees = t} = concatMap flattenTree t

{-
    Verifică dacă un arbore are conținutul dorit.
-}
treeHasContent :: (Ord p, Ord k) => BinomialTree p k -> [(p, k)] -> Bool
treeHasContent tree content = sort (flattenTree tree) == sort content

{-
    Verifică dacă o listă de arbori are conținutul dorit.
-}
treeListHasContent :: (Ord p, Ord k) => [BinomialTree p k] -> [(p, k)] -> Bool
treeListHasContent trees content = sort (concatMap flattenTree trees) == sort content

{-
    Verifică dacă un heap are conținutul dorit.
-}
heapHasContent :: (Ord p, Ord k) => BinomialHeap p k -> [(p, k)] -> Bool
heapHasContent heap content = sort (flattenHeap heap) == sort content

testAttach :: TestData
testAttach = tests 1 15
    [
        testVal "attach EmptyTree" (EmptyTree :: BinomialTree Int Char) $ attach EmptyTree EmptyTree,

        testVal "attach example 1" nodeA{children=[nodeB]} $ attach nodeA nodeB,
        testVal "attach example 2" nodeA{children=[nodeB]} $ attach nodeB nodeA,

        testVal "attach rank 1" nodeCDEF $ attach nodeCD nodeEF,
        testVal "attach rank 2" nodeGHIJKLMN $ attach nodeGHIJ nodeKLMN,

        testCond "attach larger (structure)" $ treeProperty largerAttached && getTreeSize largerAttached == 2^7,
        testCond "attach larger (contents)"  $ largerAttached `treeHasContent` largerTuples
    ]
        where
            nodeA = Node 0 'a' []
            nodeB = Node 1 'b' []

            nodeCD = Node 0 'c' [Node 1 'd' []]
            nodeEF = Node 1 'e' [Node 3 'f' []]
            nodeCDEF = nodeCD{children = nodeEF : children nodeCD}

            nodeGHIJ = Node 5 'g' [Node 5 'h' [Node 7 'j' []], Node 6 'i' []]
            nodeKLMN = Node 2 'k' [Node 3 'l' [Node 3 'n' []], Node 4 'm' []]
            nodeGHIJKLMN = nodeKLMN{children = nodeGHIJ : children nodeKLMN}

            nodeLarger1 = dummyTreeOfRank 6 0 0
            nodeLarger2 = dummyTreeOfRank 6 0 64
            largerAttached = attach nodeLarger1 nodeLarger2
            largerTuples = [(0,0),(0,64),(1,1),(1,2),(1,4),(1,8),(1,16),(1,32),(1,65),(1,66),(1,68),(1,72),(1,80),(1,96),(2,3),(2,5),(2,6),(2,9),(2,10),(2,12),(2,17),(2,18),(2,20),(2,24),(2,33),(2,34),(2,36),(2,40),(2,48),(2,67),(2,69),(2,70),(2,73),(2,74),(2,76),(2,81),(2,82),(2,84),(2,88),(2,97),(2,98),(2,100),(2,104),(2,112),(3,7),(3,11),(3,13),(3,14),(3,19),(3,21),(3,22),(3,25),(3,26),(3,28),(3,35),(3,37),(3,38),(3,41),(3,42),(3,44),(3,49),(3,50),(3,52),(3,56),(3,71),(3,75),(3,77),(3,78),(3,83),(3,85),(3,86),(3,89),(3,90),(3,92),(3,99),(3,101),(3,102),(3,105),(3,106),(3,108),(3,113),(3,114),(3,116),(3,120),(4,15),(4,23),(4,27),(4,29),(4,30),(4,39),(4,43),(4,45),(4,46),(4,51),(4,53),(4,54),(4,57),(4,58),(4,60),(4,79),(4,87),(4,91),(4,93),(4,94),(4,103),(4,107),(4,109),(4,110),(4,115),(4,117),(4,118),(4,121),(4,122),(4,124),(5,31),(5,47),(5,55),(5,59),(5,61),(5,62),(5,95),(5,111),(5,119),(5,123),(5,125),(5,126),(6,63),(6,127)]

testInsertTree :: TestData
testInsertTree = tests 2 25
    [
        testVal "insertTree example 1" [nodeA]                              treeA,
        testVal "insertTree example 2" [EmptyTree, nodeA{children=[nodeB]}] treeAB,
        testVal "insertTree example 3" [nodeC, nodeA{children=[nodeB]}]     treeABC,

        testCond "insertTree domino 11->001"    $ obeysMask 0 [0,0,1] $ insertTree (Node 0 0 []) $ dummyTrees [0,1],
        testCond "insertTree domino 101->011"   $ obeysMask 0 [0,1,1] $ insertTree (Node 0 0 []) $ dummyTrees [0,-1,2],
        testCond "insertTree domino 110->001"   $ obeysMask 0 [0,0,1] $ insertTree (Node 0 0 []) $ dummyTrees [0,1,-1],
        testCond "insertTree domino 1101->0011" $ obeysMask 0 [0,0,1,1] $ insertTree (Node 0 0 []) $ dummyTrees [0,1,-1,3],
        testCond "insertTree domino 111->0001"  $ obeysMask 4 [0,0,0,1] $ insertTree (dummyTreeOfRank 4 0 0) $ dummyTrees [4..6],

        testCond "insertTree large simple" $ obeysMask 4 (replicate 8 1) $ insertTree (dummyTreeOfRank 4 0 0) $ EmptyTree : dummyTrees [5..11],
        testCond "insertTree large domino" $ obeysMask 4 (replicate 7 0 ++ [1]) $ insertTree (dummyTreeOfRank 4 0 0) $ dummyTrees [4..10]
    ]
        where
            nodeA = Node 1 'a' []
            nodeB = Node 2 'b' []
            nodeC = Node 3 'c' []
            treeA = insertTree nodeA []
            treeAB = insertTree nodeB treeA
            treeABC = insertTree nodeC treeAB

            treesLarge = dummyTrees [4..11]

            -- |Checks if a list of binomial trees respects a binary mask where
            -- 0 represents a missing tree, and 1 represents a present tree.
            -- The trees should be valid and have the right rank.
            obeysMask :: (Ord p) => Int -> [Int] -> [BinomialTree p k] -> Bool
            obeysMask startRank mask trees = okLength && okPositions
                where
                    okLength = length mask == length trees
                    okPositions = and $ zipWith validPos [startRank..] $ zip mask trees
                    validPos _ (0, EmptyTree) = True
                    validPos rank (1, node@Node{}) = treeProperty node && treeSize node == 2^rank
                    validPos _ _ = False

            treeSize :: BinomialTree p k -> Int
            treeSize EmptyTree = 0
            treeSize Node{children=children} = 2 ^ length children

testEmptyHeap :: TestData
testEmptyHeap = tests 3 5
    [
        testCond "test emptyHeap" ((size emptyHeap == 0) && null (trees emptyHeap))
    ]

testInsert :: TestData
testInsert = tests 4 10
    [
        testCond "insert 1 element (structure)" $ binomialHeapProperty expr1,
        testCond "insert 1 element (contents)"  $ expr1 `heapHasContent` tuples1,
        testCond "insert 2 elements (structure)" $ binomialHeapProperty expr2,
        testCond "insert 2 elements (contents)"  $ expr2 `heapHasContent` tuples2,
        testCond "insert 3 elements out of order (structure)" $ binomialHeapProperty expr3,
        testCond "insert 3 elements out of order (contents)"  $ expr3 `heapHasContent` tuples3,
        testCond "insert 4 elements (structure)" $ binomialHeapProperty expr4,
        testCond "insert 4 elements (contents)"  $ expr4 `heapHasContent` tuples4,
        testCond "insert 6 elements (structure)" $ binomialHeapProperty expr5,
        testCond "insert 6 elements (contents)"  $ expr5 `heapHasContent` tuples5
    ]
        where
            expr1 = insert 1 'a' emptyHeap
            expr2 = insert 2 'b' $ insert 1 'a' emptyHeap
            expr3 = insert 2 'c' $ insert 1 'b' $ insert 3 'a' emptyHeap
            expr4 = insert 1 'a' $ insert 2 'b' $ insert 3 'c' $ insert 4 'd' emptyHeap
            expr5 = insert 100 'f' $ insert 70 'e' $ insert 200 'd' $ insert 60 'c' $ insert 80 'b' $ insert 21 'a' emptyHeap

            tuples1 = [(1,'a')]
            tuples2 = [(1,'a'),(2,'b')]
            tuples3 = [(1,'b'),(2,'c'),(3,'a')]
            tuples4 = [(1,'a'),(2,'b'),(3,'c'),(4,'d')]
            tuples5 = [(21,'a'),(60,'c'),(70,'e'),(80,'b'),(100,'f'),(200,'d')]

testFindMin :: TestData
testFindMin = tests 5 25
    [
        testVal "test findMin EmptyTree" val0 expr0,
        testVal "test findMin 3 elements" val1 expr1,
        testVal "test findMin 4 elements basic" val2 expr2,
        testVal "test findMin 4 elements insert 1 last" val3 expr3,
        testVal "test findMin 4 elements insert 1 penultimate" val4 expr4,
        testVal "test findMin 5 elements basic" val5 expr5,
        testVal "test findMin 5 elements change key order" val6 expr6,
        testVal "test findMin 5 elements change priority insertion order" val7 expr7
    ]
        where
            val0 = Nothing :: Maybe (Int, String)
            val1 = Just (1,'a')
            val2 = Just (1,'a')
            val3 = Just (1,'d')
            val4 = Just (1,'a')
            val5 = Just (1,'a')
            val6 = Just (1,'b')
            val7 = Just (1,'h')
            expr0 = findMin emptyHeap
            expr1 = findMin $ BinomialHeap 3 [Node 3 'c' [], Node 1 'a' [Node 2 'b' []]]
            expr2 = findMin $ BinomialHeap 4 [EmptyTree, EmptyTree, Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]]
            expr3 = findMin $ BinomialHeap 4 [EmptyTree, EmptyTree, Node 1 'd' [Node 2 'a' [Node 3 'b' []], Node 4 'c' []]]
            expr4 = findMin $ BinomialHeap 4 [EmptyTree, EmptyTree, Node 1 'a' [Node 2 'b' [Node 3 'c' []], Node 4 'd' []]]
            expr5 = findMin $ BinomialHeap 5 [Node 5 'e' [], EmptyTree, Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]]
            expr6 = findMin $ BinomialHeap 5 [Node 5 'h' [], EmptyTree, Node 1 'b' [Node 3 'g' [Node 4 'f' []], Node 2 'a' []]]
            expr7 = findMin $ BinomialHeap 5 [Node 1 'h' [], EmptyTree, Node 2 'f' [Node 4 'a' [Node 5 'b' []], Node 3 'g' []]]

testMergeTrees :: TestData
testMergeTrees = tests 7 30
    [
        testCond "mergeTrees empty lists (structure)" $ checkResult expr1,
        testCond "mergeTrees empty lists (contents)"  $ expr1 `treeListHasContent` tuples1,
        testCond "mergeTrees empty trees (structure)" $ checkResult expr2,
        testCond "mergeTrees empty trees (contents)"  $ expr2 `treeListHasContent` tuples2,
        testCond "mergeTrees rank 0 (structure)" $ checkResult expr3,
        testCond "mergeTrees rank 0 (contents)"  $ expr3 `treeListHasContent` tuples3,
        testCond "mergeTrees rank 1 - no carry (structure)" $ checkResult expr4,
        testCond "mergeTrees rank 1 - no carry (contents)"  $ expr4 `treeListHasContent` tuples4,
        testCond "mergeTrees rank 1 - carry (structure)" $ checkResult expr5,
        testCond "mergeTrees rank 1 - carry (contents)"  $ expr5 `treeListHasContent` tuples5,
        testCond "mergeTrees rank 2 - first longer (structure)" $ checkResult expr6,
        testCond "mergeTrees rank 2 - first longer (contents)"  $ expr6 `treeListHasContent` tuples6,
        testCond "mergeTrees rank 2 - second longer (structure)" $ checkResult expr7,
        testCond "mergeTrees rank 2 - second longer (contents)"  $ expr7 `treeListHasContent` tuples7,
        testCond "mergeTrees rank 2 - no carry (structure)" $ checkResult expr8,
        testCond "mergeTrees rank 2 - no carry (contents)"  $ expr8 `treeListHasContent` tuples8,
        testCond "mergeTrees rank 3 - equal priorities (structure)" $ checkResult expr9,
        testCond "mergeTrees rank 3 - equal priorities (contents)"  $ expr9 `treeListHasContent` tuples9,
        testCond "mergeTrees rank 3 - carry (structure)" $ checkResult expr10,
        testCond "mergeTrees rank 3 - carry (contents)"  $ expr10 `treeListHasContent` tuples10
    ]
        where
            expr1 = mergeTrees [] [] :: [BinomialTree Int Int]
            expr2 = mergeTrees [EmptyTree] [EmptyTree] :: [BinomialTree Int Int]
            expr3 = mergeTrees [Node 1 2 []] [Node 2 4 []]
            expr4 = mergeTrees [EmptyTree, Node 1 'a' [Node 5 'b' []]] [Node 9 'c' []]
            expr5 = mergeTrees [EmptyTree, Node 4 7.98 [Node 7.5 12.3 []]] [Node 0.2 3.08 [], Node 21.38 43.8 [Node 74.28 3.9 []]]
            expr6 = mergeTrees [EmptyTree, dummyTreeOfRank 1 3 4, dummyTreeOfRank 2 5 2] [Node 5 7 [], dummyTreeOfRank 1 4 6]
            expr7 = mergeTrees [EmptyTree, Node 3 9 [Node 4 11 []]] [Node 2 5 [], Node 7 20 [Node 8 12 []], dummyTreeOfRank 2 6 4]
            expr8 = mergeTrees [Node 1 5 [], EmptyTree, dummyTreeOfRank 2 56 73] [EmptyTree, Node 11 22 [Node 33 44 []]]
            expr9 = mergeTrees tree1 tree2
                where
                    tree1 = [EmptyTree, Node 1 5 [Node 2 3 []], dummyTreeOfRank 2 8 (-1)]
                    tree2 = [Node 10 (-2) [], Node 1 8 [Node 2 15 []], dummyTreeOfRank 2 9 47, dummyTreeOfRank 3 98 23]
            expr10 = mergeTrees tree1' tree2'
                where
                    tree1' = [Node 4 8 [], Node 7 12 [Node 8 11 []], dummyTreeOfRank 2 5 (-3), dummyTreeOfRank 3 10 6]
                    tree2' = [Node 2 1 [], Node 9 10 [Node 19 29 []], dummyTreeOfRank 2 18 9, dummyTreeOfRank 3 6 100]

            tuples1 = []
            tuples2 = []
            tuples3 = [(1,2),(2,4)]
            tuples4 = [(1,'a'),(5,'b'),(9,'c')]
            tuples5 = [(0.2,3.08),(4.0,7.98),(7.5,12.3),(21.38,43.8),(74.28,3.9)]
            tuples6 = [(3,4),(4,5),(4,6),(5,2),(5,7),(5,7),(6,3),(6,4),(7,5)]
            tuples7 = [(2,5),(3,9),(4,11),(6,4),(7,5),(7,6),(7,20),(8,7),(8,12)]
            tuples8 = [(1,5),(11,22),(33,44),(56,73),(57,74),(57,75),(58,76)]
            tuples9 = [(1,5),(1,8),(2,3),(2,15),(8,-1),(9,0),(9,1),(9,47),(10,-2),(10,2),(10,48),(10,49),(11,50),(98,23),(99,24),(99,25),(99,27),(100,26),(100,28),(100,29),(101,30)]
            tuples10 = [(2,1),(4,8),(5,-3),(6,-2),(6,-1),(6,100),(7,0),(7,12),(7,101),(7,102),(7,104),(8,11),(8,103),(8,105),(8,106),(9,10),(9,107),(10,6),(11,7),(11,8),(11,10),(12,9),(12,11),(12,12),(13,13),(18,9),(19,10),(19,11),(19,29),(20,12)]

testMerge :: TestData
testMerge = tests 8 10
    [
        testCond "merge 1 (structure)" $ binomialHeapProperty expr1,
        testCond "merge 1 (contents)"  $ expr1 `heapHasContent` tuples1,
        testCond "merge 2 (structure)" $ binomialHeapProperty expr2,
        testCond "merge 2 (contents)"  $ expr2 `heapHasContent` tuples2,
        testCond "merge 3 (structure)" $ binomialHeapProperty expr3,
        testCond "merge 3 (contents)"  $ expr3 `heapHasContent` tuples3,
        testCond "merge 4 (structure)" $ binomialHeapProperty expr4,
        testCond "merge 4 (contents)"  $ expr4 `heapHasContent` tuples4,
        testCond "merge 5 (structure)" $ binomialHeapProperty expr5,
        testCond "merge 5 (contents)"  $ expr5 `heapHasContent` tuples5
    ]
        where
            expr1 = merge (BinomialHeap 1 [Node 5 8 []]) (BinomialHeap 2 [EmptyTree, Node 5 9 [Node 13 11 []]])
            expr2 = merge (BinomialHeap 3 [Node 0 3 [], Node 4 11 [Node 5 19 []]]) (BinomialHeap 2 [EmptyTree, Node 98 24 [Node 102 118 []]])
            expr3 = merge (dummyHeap [0, 1, -1, 3, 4]) (dummyHeap [-1, 1, 2, -1, 4, 5])
            expr4 = merge (dummyHeap [0, -2, 2, 3, -1, -5, 6]) (dummyHeap [-1, 1, 2, -3, -1, 5, -6, 7])
            expr5 = merge (dummyHeap ([-1, -1] ++ [2..4])) (dummyHeap ([-1, -1, -1] ++ [3..5]))

            tuples1 = [(5,8),(5,9),(13,11)]
            tuples2 = [(0,3),(4,11),(5,19),(98,24),(102,118)]
            tuples3 = [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(1,1),(1,1),(1,1),(1,1),(1,1),(1,1),(1,1),(1,2),(1,2),(1,2),(1,2),(1,2),(1,4),(1,4),(1,4),(1,4),(1,8),(1,8),(1,8),(1,16),(2,3),(2,3),(2,3),(2,3),(2,3),(2,5),(2,5),(2,5),(2,5),(2,6),(2,6),(2,6),(2,6),(2,9),(2,9),(2,9),(2,10),(2,10),(2,10),(2,12),(2,12),(2,12),(2,17),(2,18),(2,20),(2,24),(3,7),(3,7),(3,7),(3,7),(3,11),(3,11),(3,11),(3,13),(3,13),(3,13),(3,14),(3,14),(3,14),(3,19),(3,21),(3,22),(3,25),(3,26),(3,28),(4,15),(4,15),(4,15),(4,23),(4,27),(4,29),(4,30),(5,31)]
            tuples4 = [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(1,1),(1,1),(1,1),(1,1),(1,1),(1,1),(1,1),(1,2),(1,2),(1,2),(1,2),(1,2),(1,2),(1,4),(1,4),(1,4),(1,4),(1,8),(1,8),(1,8),(1,16),(1,16),(1,16),(1,32),(1,32),(1,64),(2,3),(2,3),(2,3),(2,3),(2,3),(2,3),(2,5),(2,5),(2,5),(2,5),(2,6),(2,6),(2,6),(2,6),(2,9),(2,9),(2,9),(2,10),(2,10),(2,10),(2,12),(2,12),(2,12),(2,17),(2,17),(2,17),(2,18),(2,18),(2,18),(2,20),(2,20),(2,20),(2,24),(2,24),(2,24),(2,33),(2,33),(2,34),(2,34),(2,36),(2,36),(2,40),(2,40),(2,48),(2,48),(2,65),(2,66),(2,68),(2,72),(2,80),(2,96),(3,7),(3,7),(3,7),(3,7),(3,11),(3,11),(3,11),(3,13),(3,13),(3,13),(3,14),(3,14),(3,14),(3,19),(3,19),(3,19),(3,21),(3,21),(3,21),(3,22),(3,22),(3,22),(3,25),(3,25),(3,25),(3,26),(3,26),(3,26),(3,28),(3,28),(3,28),(3,35),(3,35),(3,37),(3,37),(3,38),(3,38),(3,41),(3,41),(3,42),(3,42),(3,44),(3,44),(3,49),(3,49),(3,50),(3,50),(3,52),(3,52),(3,56),(3,56),(3,67),(3,69),(3,70),(3,73),(3,74),(3,76),(3,81),(3,82),(3,84),(3,88),(3,97),(3,98),(3,100),(3,104),(3,112),(4,15),(4,15),(4,15),(4,23),(4,23),(4,23),(4,27),(4,27),(4,27),(4,29),(4,29),(4,29),(4,30),(4,30),(4,30),(4,39),(4,39),(4,43),(4,43),(4,45),(4,45),(4,46),(4,46),(4,51),(4,51),(4,53),(4,53),(4,54),(4,54),(4,57),(4,57),(4,58),(4,58),(4,60),(4,60),(4,71),(4,75),(4,77),(4,78),(4,83),(4,85),(4,86),(4,89),(4,90),(4,92),(4,99),(4,101),(4,102),(4,105),(4,106),(4,108),(4,113),(4,114),(4,116),(4,120),(5,31),(5,31),(5,31),(5,47),(5,47),(5,55),(5,55),(5,59),(5,59),(5,61),(5,61),(5,62),(5,62),(5,79),(5,87),(5,91),(5,93),(5,94),(5,103),(5,107),(5,109),(5,110),(5,115),(5,117),(5,118),(5,121),(5,122),(5,124),(6,63),(6,63),(6,95),(6,111),(6,119),(6,123),(6,125),(6,126),(7,127)]
            tuples5 = [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(1,1),(1,1),(1,1),(1,1),(1,1),(1,1),(1,2),(1,2),(1,2),(1,2),(1,2),(1,2),(1,4),(1,4),(1,4),(1,4),(1,4),(1,8),(1,8),(1,8),(1,16),(2,3),(2,3),(2,3),(2,3),(2,3),(2,3),(2,5),(2,5),(2,5),(2,5),(2,5),(2,6),(2,6),(2,6),(2,6),(2,6),(2,9),(2,9),(2,9),(2,10),(2,10),(2,10),(2,12),(2,12),(2,12),(2,17),(2,18),(2,20),(2,24),(3,7),(3,7),(3,7),(3,7),(3,7),(3,11),(3,11),(3,11),(3,13),(3,13),(3,13),(3,14),(3,14),(3,14),(3,19),(3,21),(3,22),(3,25),(3,26),(3,28),(4,15),(4,15),(4,15),(4,23),(4,27),(4,29),(4,30),(5,31)]

main :: IO ()
main = vmCheck [ testAttach
               , testInsertTree
               , testEmptyHeap
               , testInsert
               , testFindMin
               , testMergeTrees
               , testMerge
               ]
