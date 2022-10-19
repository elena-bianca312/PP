{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data D = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-lin cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}

data Cell = BlankCell | HunterCell | TargetCell Target | ObstacleCell | GatewayCell 
    deriving (Eq, Ord)

instance Show Cell where
    show cell = case cell of
        BlankCell        -> " "
        HunterCell       -> "!"
        TargetCell _     -> "*"
        ObstacleCell     -> "@"
        GatewayCell      -> "#"

data Game = Game Int Int [Cell] [(Position, Position)] [Target]
    deriving (Eq, Ord)

{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}
gameAsString :: Game -> String
gameAsString (Game l c cells _ _)
    | l == 1 = intercalate "" (map show $ take c cells)
    | otherwise = first ++ "\n" ++ rest where
        first = intercalate "" (map show $ take c cells)
        rest = gameAsString (Game (l - 1) c (drop c cells) [] [])

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}

emptyGame :: Int -> Int -> Game
emptyGame lin col = Game lin col (replicate col ObstacleCell ++                                                                      -- the first row
                            [ObstacleCell] ++ [HunterCell] ++ replicate (col - 3) BlankCell ++ [ObstacleCell] ++                     -- the second row
                            concat (replicate (lin - 3) ([ObstacleCell] ++ replicate (col - 2) BlankCell ++ [ObstacleCell])) ++      -- the other rows
                            replicate col ObstacleCell) [] []                                                                        -- the last row


{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}

-- deleteHunter length (cells of game) 0 game
deleteHunter :: Int -> Int -> Game -> Game
deleteHunter acc pos (Game l c cells list targets) = case acc == 0 of
    False -> deleteHunter (acc - 1) (pos + 1) (Game l c (take pos cells ++ newCell ++ drop (pos + 1) cells) list targets) where
          newCell
               | take 1 (drop pos cells) == [HunterCell] = [BlankCell]
               | otherwise = take 1 (drop pos cells)
    _     -> Game l c cells list targets

addHunterHelper :: Position -> Game -> Game
addHunterHelper (x, y) (Game l c cells list targets) = Game l c (take n cells ++ newCell ++ drop (n + 1) cells) list targets
    where
        n = x * c + y
        newCell
            | take 1 (drop n cells) == [BlankCell] = [HunterCell]
            | take 1 (drop n cells) == [GatewayCell] = [HunterCell]
            | otherwise = take 1 (drop n cells)

addHunter :: Position -> Game -> Game
addHunter (x, y) (Game l c cells list targets) =
    if cell == [BlankCell] || cell == [GatewayCell]
        then addHunterHelper (x, y) (deleteHunter (l * c) 0 (Game l c cells list targets))
        else Game l c cells list targets
        where cell = take 1 (drop (x * c + y) cells)



{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget behav (x, y) (Game l c cells list targets) = Game l c (take n cells ++ newCell ++ drop (n + 1) cells) list (targets ++ [Target (x, y) behav])
    where
        n = x * c + y
        newCell
            | take 1 (drop n cells) == [BlankCell] = [TargetCell (Target (x, y) behav)]
            | take 1 (drop n cells) == [GatewayCell] = [TargetCell (Target (x, y) behav)]
            | otherwise = take 1 (drop n cells) 

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway ((x1, y1), (x2, y2)) (Game l c cells list targets) = Game l c (take mini cells ++
                                                        minCell ++
                                                        take (maxi - mini - 1) (drop (mini + 1) cells) ++
                                                        maxCell ++
                                                        drop (maxi + 1) cells) (list ++ link1 ++ link2) targets
    where
        pos1 = x1 * c + y1
        pos2 = x2 * c + y2
        mini = minimum [pos1, pos2]
        maxi = maximum [pos1, pos2]
        cond1 = take 1 (drop mini cells) == [BlankCell] || maxi == c + 1 || mini == c + 1
        cond2 = take 1 (drop maxi cells) == [BlankCell] || maxi == c + 1 || mini == c + 1
        minCell
            | cond1 && cond2 = [GatewayCell]
            | otherwise = take 1 (drop mini cells)
        maxCell
            | cond1 && cond2 = [GatewayCell]
            | otherwise = take 1 (drop maxi cells)
        link1
            | cond1 && cond2 = [((x1, y1), (x2, y2))]
            | otherwise = []
        link2
            | cond1 && cond2 = [((x2, y2), (x1, y1))]
            | otherwise = []

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle (x, y) (Game l c cells list targets) = Game l c (take n cells ++ newCell ++ drop (n + 1) cells) list targets
    where
        n = x * c + y
        newCell
            | take 1 (drop n cells) == [BlankCell] = [ObstacleCell]
            | otherwise = take 1 (drop n cells)

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}

reconstructGateways :: Game -> Game -> Game
reconstructGateways game (Game _ _ _ [] _) = game
reconstructGateways (Game l1 c1 cells1 list1 targets1) (Game l2 c2 cells2 list2 targets2) =
        reconstructGateways (Game l1 c1 (take n cells1 ++ newCell ++ drop (n + 1) cells1) list1 targets1) (Game l2 c2 cells2 (tail list2) targets2)
            where
                a = fst $ head list2
                n = fst a * c1 + snd a
                newCell = if take 1 (drop n cells1) == [BlankCell]
                    then [GatewayCell]
                    else take 1 (drop n cells1)

findGatewayPair :: Position -> Game -> Maybe Position
findGatewayPair _ (Game _ _ _ [] _) = Nothing
findGatewayPair (x, y) (Game l c cells list targets) 
    | (x, y) == fst (head list) = Just (snd (head list))
    | otherwise = findGatewayPair (x, y) (Game l c cells (tail list) targets)

attemptMove :: Position -> Game -> Maybe Position
attemptMove (x, y) (Game l c cells list targets) 
    | take 1 (drop (x * c + y) cells) == [BlankCell] = case findGatewayPair (x, y) (Game l c cells list targets) /= Nothing of
        True -> findGatewayPair (x, y) (Game l c cells list targets)
        _    -> Just (x, y)
    | take 1 (drop (x * c + y) cells) == [GatewayCell] = findGatewayPair (x, y) (Game l c cells list targets)
    | take 1 (drop (x * c + y) cells) == [ObstacleCell] = Nothing
    | otherwise = Nothing 

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

getMaybeValue :: Maybe Position -> Position
getMaybeValue x = case x of
    Nothing -> (0, 0)
    Just val -> val

goDirection :: Behavior -> Position -> Behavior
goDirection behav attemptedPos currentPos game = 
    if attemptMove attemptedPos game /= Nothing 
        then Target pos1 behav
        else Target pos2 behav
            where
                pos1 = getMaybeValue $ attemptMove attemptedPos game
                pos2 = getMaybeValue $ attemptMove currentPos game

goEast :: Behavior
goEast (x, y) = goDirection goEast (x, y + 1) (x, y)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest (x, y) = goDirection goWest (x, y - 1) (x, y)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth (x, y) = goDirection goNorth (x - 1, y) (x, y)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth (x, y) = goDirection goSouth (x + 1, y) (x, y) 

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}

bounce :: Int -> Behavior               
bounce direction (x, y) game =
    if attemptMove (x + direction, y) game /= Nothing 
        then Target pos1 (bounce direction)
        else Target pos2 (bounce (negate direction))
            where
                pos1 = getMaybeValue $ attemptMove (x + direction, y) game
                pos2 = getMaybeValue $ attemptMove (x - direction, y) game

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}

movementTypeTarget :: Target -> Game -> Target
movementTypeTarget (Target (x, y) behav) = behav (x, y) 

deleteAllTargetsPositions :: Int -> Game -> [Cell]
deleteAllTargetsPositions 0 (Game _ _ cells _ _) = cells
deleteAllTargetsPositions n (Game l c cells list targets) 
    = deleteAllTargetsPositions (n - 1) (Game l c (take n cells ++ newCell ++ drop (n + 1) cells) list targets)
    where
        newCell
            | take 1 (drop n cells) == [GatewayCell] = [GatewayCell]
            | take 1 (drop n cells) == [ObstacleCell] = [ObstacleCell]
            | take 1 (drop n cells) == [HunterCell] = [HunterCell]
            | otherwise = [BlankCell]

moveTargetsHelper :: Game -> Game -> Game
moveTargetsHelper game1 (Game _ _ _ _ []) = reconstructGateways game1 game1
moveTargetsHelper (Game l1 c1 cells1 list1 targets1) game2@(Game l2 c2 cells2 list2 targets2) = 
    moveTargetsHelper (addTarget b pos (Game l1 c1 newCells list1 (tail targets1))) (Game l2 c2 cells2 list2 (tail targets2))
        where 
            Target pos b = movementTypeTarget (head targets2) game2             -- new Target updated position
            Target (x, y) _ = head targets2                                     -- old Target old position
            oldPos = x * c1 + y    
            newCells                                                            -- remove old TargetCell from Array
                | take 1 (drop oldPos cells1) == [GatewayCell] = cells2
                | otherwise = take oldPos cells1 ++ [BlankCell] ++ drop (oldPos + 1) cells1
    

moveTargets :: Game -> Game
moveTargets game@(Game l c _ list targets) = moveTargetsHelper game (Game l c (deleteAllTargetsPositions (l * c) game) list targets) 

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}

-- param1: hunter position && param2: target position
adjacentPositions1 :: Position -> Position -> Bool 
adjacentPositions1 (x1, y1) (x2, y2) 
    | x2 == x1 - 1 && y2 == y1 = True 
    | x2 == x1 + 1 && y2 == y1 = True 
    | x2 == x1 && y2 == y1 - 1 = True 
    | x2 == x1 && y2 == y1 + 1 = True
    | otherwise = False

isTargetKilled :: Position -> Target -> Bool
isTargetKilled (x, y) (Target (a, b) _) = adjacentPositions1 (x, y) (a, b)


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul D reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}

-- n = l * c
getCurrentPositionHunter :: Int -> Game -> Position
getCurrentPositionHunter 0 _ = (1, 1)
getCurrentPositionHunter n (Game l c cells list targets) = 
    if take 1 (drop n cells) == [HunterCell]
        then (n `div` c, n `mod` c)
        else getCurrentPositionHunter (n - 1) (Game l c cells list targets)

getPositionTarget :: Target -> Position
getPositionTarget target = pos
    where Target pos _ = target

goHunter :: Position -> Position -> Game -> Position
goHunter attemptedPos currentPos game = 
    if attemptMove attemptedPos game /= Nothing 
        then pos1
        else pos2
            where
                pos1 = getMaybeValue $ attemptMove attemptedPos game 
                pos2 = getMaybeValue $ attemptMove currentPos game 


moveHunter :: D -> Game -> Game
moveHunter direction game@(Game l c _ _ _) 
    | direction == North = addHunter (goHunter (x - 1, y) (x, y) game) game
    | direction == South = addHunter (goHunter (x + 1, y) (x, y) game) game
    | direction == East  = addHunter (goHunter (x, y + 1) (x, y) game) game
    | direction == West  = addHunter (goHunter (x, y - 1) (x, y) game) game
    | otherwise = addHunter (x, y) game
    where
        (x, y) = getCurrentPositionHunter (l * c) game

updateTargetList :: Position -> [Target] -> [Target]
updateTargetList _ [] = []
updateTargetList pos (l:ls) 
    | pos == getPositionTarget l = ls
    | otherwise = l : updateTargetList pos ls

-- param1 = target position && param2 = game
killTargetAtPosition :: Position -> Game -> Game
killTargetAtPosition (a, b) (Game l c cells list targets) = Game l c (take n cells ++ newCell ++ drop (n + 1) cells) list newTargets
    where
        n = a * c + b
        newCell = [BlankCell]
        newTargets = updateTargetList (a, b) targets

-- param1 = hunter position && param2 = game && param3 = game
killTargets :: Position -> Game -> Game -> Game
killTargets _ game (Game _ _ _ _ []) = game
killTargets (x, y) game (Game l2 c2 cells2 list2 targets2) 
    | isTargetKilled (x, y) (head targets2) = 
        killTargets (x, y) 
                    (killTargetAtPosition (getPositionTarget $ head targets2) game) 
                    (Game l2 c2 cells2 list2 (tail targets2))
    | otherwise = killTargets (x, y) game (Game l2 c2 cells2 list2 (tail targets2)) 

advanceGameState :: D -> Bool -> Game -> Game
advanceGameState direction bool game@(Game l c _ _ _)
    | bool = reconstructGateways game4 game4
    | otherwise = reconstructGateways game1 game1
        where
            hunterPosition = getCurrentPositionHunter (l * c) game1
            game1 = moveHunter direction game
            game2 = killTargets hunterPosition game1 game1
            game3 = moveTargets game2
            game4 = killTargets hunterPosition game3 game3


{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft (Game _ _ _ _ targets) 
    | null targets = False 
    | otherwise = True


{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}

circleValidPos :: Position -> Game -> Behavior -> Target
circleValidPos pos game func = if attemptMove pos game /= Nothing 
    then Target (getMaybeValue $ attemptMove pos game) func
    else Target pos func

circleHelper :: String -> Int -> Int -> Int -> Behavior
circleHelper direction seqv stopH stopD (x, y) game
    | direction == "goEast"      && seqv /= stopH
        = circleValidPos (x, y + 1) game (circleHelper "goEast" (seqv + 1) stopH stopD)
    | direction == "goEast"      && seqv == stopH
        = circleValidPos (x + 1, y) game (circleHelper "goSouth" 0 stopH stopD)
    | direction == "goSouth"     && seqv /= stopH
        = circleValidPos (x + 1, y) game (circleHelper "goSouth" (seqv + 1) stopH stopD)
    | direction == "goSouth"     && seqv == stopH
        = circleValidPos (x, y - 1) game (circleHelper "goWest" 0 stopH stopD)
    | direction == "goWest"      && seqv /= stopH
        = circleValidPos (x, y - 1) game (circleHelper "goWest" (seqv + 1) stopH stopD)
    | direction == "goWest"      && seqv == stopH
        = circleValidPos (x - 1, y) game (circleHelper "goNorth" 0 stopH stopD) 
    | direction == "goNorth"     && seqv /= stopH
        = circleValidPos (x - 1, y) game (circleHelper "goNorth" 0 stopH stopD) 
    | direction == "goNorth"     && seqv == stopH
        = circleValidPos (x - 1, y) game (circleHelper "goNorth" 0 stopH stopD) 
    | otherwise = Target (x, y) (circleHelper "goSouth" 0 stopH stopD)

circle :: Position -> Int -> Behavior

circle (cx, cy) radius (x, y) = circleHelper "goEast" 0 1 1 (x, y) 

instance ProblemState Game D where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game = list
        where 
            actionNorth = advanceGameState North False game
            actionSouth = advanceGameState South False game
            actionEast  = advanceGameState East  False game
            actionWest  = advanceGameState West  False game
            list = [(North, actionNorth)] ++ [(South, actionSouth)] ++ [(East, actionEast)] ++ [(West, actionWest)]

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}

    isGoal (Game _ _ _ _ []) = False
    isGoal game@(Game l c cells list targets) = isTargetKilled pos (head targets) || isGoal (Game l c cells list (tail targets))
        where
            pos = getCurrentPositionHunter (l * c) game

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}

    h (Game _ _ _ _ []) = 10000             -- unreachable
    h game@(Game l c cells list targets) =
        minimum (hEuclidean posH posT, h (Game l c cells list (tail targets)))
            where 
                posH = getCurrentPositionHunter (l * c) game
                posT = getPositionTarget $ head targets

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ (x1 - x2) ^ pow + (y1 - y2) ^ pow
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame D where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
