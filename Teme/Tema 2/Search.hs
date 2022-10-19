{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.PSQueue as PQ
import Data.Maybe
import Prelude
import qualified Data.Set as S

{-
    *** TODO ***
    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:
    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime;
    * estimarea costului până la starea finală;
    * copiii, ce vor desemna stările învecinate;
-}

data Node s a = NodeConstructor s (Maybe a) (Maybe (Node s a)) Int Float [Node s a] 

{-
    *** TODO ***
    Instanțiați Eq și Ord pe baza stării.
-}

instance Eq s => Eq (Node s a) where
    NodeConstructor s1 _ _ _ _ _ ==  NodeConstructor s2 _ _ _ _ _ = s1 == s2

instance Ord s => Ord (Node s a) where
    NodeConstructor s1 _ _ _ _ _ <=  NodeConstructor s2 _ _ _ _ _ = s1 <= s2

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState (NodeConstructor s _ _ _ _ _) = s

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (NodeConstructor _ _ parent _ _ _) = parent

nodeDepth :: Node s a -> Int
nodeDepth (NodeConstructor _ _ _ depth _ _) = depth

nodeChildren :: Node s a -> [Node s a]
nodeChildren (NodeConstructor _ _ _ _ _ children) = children

nodeHeuristic :: Node s a -> Float
nodeHeuristic (NodeConstructor _ _ _ _ cost _) = cost

nodeAction :: Node s a -> Maybe a
nodeAction (NodeConstructor _ a _ _ _ _) = a

{-
    *** TODO ***
    Generarea întregului spațiu al stărilor.
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente, și așa mai
    departe, recursiv.
-}

createStateSpaceHelper :: (ProblemState s a, Eq s) => s -> Maybe a -> Maybe (Node s a) -> Int -> Node s a
createStateSpaceHelper state action parent depth = nod
    where
        nod = NodeConstructor state action parent depth (h state) (map (\(x, y) -> createStateSpaceHelper y (Just x) (Just nod) (depth + 1)) (successors state))


createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initialState = createStateSpaceHelper initialState Nothing Nothing 0

{-
    Funcție ce primește o coadă de priorități și întoarce o pereche
    formată din cheia cu prioritatea minimă și coada din care a fost ștearsă
    aceasta.
    Hint: O puteți folosi pentru a extrage și a șterge un nod din frontieră.
-}

deleteFindMin :: (Ord k, Ord p) => PQ.PSQ k p -> (k, PQ.PSQ k p)
deleteFindMin pq = (minK, pq')
    where minK = PQ.key $ fromJust $ PQ.findMin pq
          pq' = PQ.deleteMin pq

{-
    *** TODO ***
    Primește nodul curent și mulțimea stărilor vizitate și întoarce
    o listă cu nodurile succesor nevizitate, care ar putea fi introduse
    în frontieră.
-}

suitableSuccs :: (ProblemState s a, Ord s) => Node s a -> S.Set s -> [Node s a]
suitableSuccs node visited = filter (\x -> S.notMember (nodeState x) visited) (nodeChildren node)

{-
    *** TODO ***
    Primește o frontieră (o coadă de priorități) și un nod ce trebuie inserat în aceasta,
    întorcând o nouă frontieră.
    ATENȚIE: Dacă la introducerea unui nod există deja în frontieră un alt nod cu aceeași
    stare, dar cu cost mai mare, nodul nou, cu cost mai mic îl va înlocui pe cel vechi.
    
    Hints:
    1. Vedeți funcția insertWith din pachetul PSQueue.
        (https://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html#v:insertWith)
    2. Costul se calculează ca suma dintre adâncime și euristică.
-}

insertSucc :: (ProblemState s a, Ord s) => PQ.PSQ (Node s a) Float -> Node s a -> PQ.PSQ (Node s a) Float
insertSucc frontier node = PQ.insertWith min node (nodeHeuristic node + fromIntegral (nodeDepth node)) frontier

{-
    *** TODO ***
    Primește nodul curent, frontiera și mulțimea stărilor vizitate, întorcând noua
    frontieră (coadă de priorități) în care au fost adăugate nodurile succesor validate
    de suitableSuccs.
-}

insertSuccs :: (ProblemState s a, Ord s) => Node s a -> PQ.PSQ (Node s a) Float -> S.Set s -> PQ.PSQ (Node s a) Float
insertSuccs node frontier visited = foldl insertSucc frontier x
    where
        x = suitableSuccs node visited

{-
    *** TODO ***
    Funcție helper care implementează A-star.
    Primește o mulțime de noduri vizitate și o coadă de priorități (aka frontiera) și
    întoarce starea finală.
    Se procedează astfel până la întâlnirea unei stări scop:
        - se extrage un nod adecvat din frontieră
        - se marchează starea acestuia ca fiind vizitată
        - se introduc succesorii în frontieră
-}

astar' :: (ProblemState s a, Ord s) => S.Set s -> PQ.PSQ (Node s a) Float -> Node s a
astar' visited frontier
    | PQ.null finalFrontier = node
    | isGoal (nodeState node) = node
    | otherwise = astar' newVisited finalFrontier
    where
        node        = fst $ deleteFindMin frontier
        newFrontier = snd $ deleteFindMin frontier
        newVisited  = S.insert (nodeState node) visited
        finalFrontier = insertSuccs node newFrontier newVisited

{-
    *** TODO ***
  
    Primește starea inițială și întoarce starea finală pentru o singură aplicare
    a algoritmului.
    Asigură parametrii inițiali corecți pentru aplicarea funcției astar'.
-}

astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode = astar' S.empty (PQ.insert initialNode cost PQ.empty)
    where
        cost = (h $ nodeState initialNode) + (fromIntegral $ nodeDepth initialNode)

{-
    *** TODO ***
    Pornind de la un nod, reface parțial calea către nodul inițial, urmând legăturile
    către părinți.
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea următoare
    stării inițiale și se încheie la starea finală.
    ATENȚIE: Nodul inițial este singurul exclus!
-}

extractPath :: Node s a -> [(a, s)]
extractPath goalNode
    | isNothing $ nodeAction x = []
    | isNothing $ nodeParent x = [(fromJust $ nodeAction x, nodeState x)]
    | otherwise = extractPath (fromJust $ nodeParent x) ++ [(fromJust $ nodeAction x, nodeState x)]
        where x = goalNode

