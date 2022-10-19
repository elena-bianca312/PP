# Tema 2 (Haskell) - Paradigme de programare
# Dumitru Elena Bianca
# 324CD


## Basics.hs

### Definirea datelor:

- data Cell: Inspirat din logica laboratorului 3, aici am definit tipul pentru
fiecare celula pentru a putea defini instantierea de show in functie de tipul
celulei. Astfel, avem definite: BlankCell, HunterCell, TargetCell, ObstacleCell
si GatewayCell.
- data Game: aceasta definitie contine informatii despre elementele jocului pentru
a putea putea construi mai departe regulile si miscarile pentru fiecare compo-
nenta in parte. Astfel, in Game retinem numarul de linii, numarul de coloane,
un array de Cell-uri (care reprezinta efectiv starea jocului), o lista de 
perechi cu elemente de tip Position pentru a retine legaturile dintre Gateways 
si o lista de Targets pentru a tine evidenta tuturor Targeturilor din joc.
Obs: Pentru Target m-am folosit de tipul deja predefinit care contine informatii
despre Position si Behavior.


### Instance Show

Pentru a defini comportamentul clasei Show pe tipul nostru de date, am definint
functia gameAsString in mod recursiv astfel:	
	- Pentru fiecare linie, afisam continutul:
		- Luam "nr de coloane" celule carora le facem show. Deoarece
		le-am transformat intr-un tot unitar, ele vor fi afisate cu ",".
		Asadar facem intercalate cu "" pentru a avea formatul pe care il
		dorim
		- Apelam functia recursiv pe restul de celule (deci pe Game fara
		primele "nr de coloane" celule, decrementand numarul de linii
		pana cand ajungem la cazul de baza unde ne oprim

### Functii tema

Adaugare elemente:

- emptyGame: pentru aceasta functie am definit in mod explicit tabla de joc dupa
	definitia din enunt. 
	Pe prima linie, am adaugat "nr de coloane" celule tip Obstacol.
	Pe a doua linie, am adaugat o celula Obstacol, celula Hunter pe pozitia
	initiala, iar apoi am completat restul jocului cu (c - 3) celule Blank
	si inca o celula obstacol.
	Pe urmatoarele (l - 3) linii am adaugat celule obstacol in margini si
	restul blank
	Pe ultima linie am adaugat iar "nr de coloane" celule tip Obstacol.
- addHunter: Daca celula pe care adaugam hunterul este blank sau gateway (inseamna
	ca se va teleporta) inseamna ca putem adauga hunterul. In acest caz, 
	trebuie sa eliminam vechea pozitie (pentru a nu coexista 2 hunteri) asa
	ca, inainte de a adauga hunterul, am sters toti hunterii existenti deja 
	in joc (apeland functia deleteHunter). Apoi am inlocuit celula respecti-
	va cu Hunterul.
	Altfel, lasam jocul asa cum este deoarece hunterul nu poate ajunge in
	acea pozitie
- addTarget: Functia aceasta are un comportament asemanator deoarece verificam
	daca pozitia este valida (este Blank sau Gateway) si doar adaugam 
	Targetul. Cand il adaugam, trebuie sa definim noi un "nou obiect" cu po-
	zitia si behaviour-ul primite ca parametru. De asemenea, adaugam target-
	ul in lista de targets, camp al lui Game
- addGateway: In aceasta functie primim 2 gateway-uri care trebuie adaugate si a
	caror legatura trebuie sa o retinem. Astfel, pentru o insertie corecta
	trebuie sa vedem care pozitie este prima, respectiv a doua (mini si 
	maxi) si sa verificam daca ambele pozitii sunt valide.
	De asemenea, deoarece intr-un test trebuia sa adaugam gateways intr-un
	emptyGame, am ales si cazul in care pozitia este ocupata de hunterul
	default (1,1) cu indexul (c + 1) sa fie valid.
	Am adaugat apoi noile celule si am facut update la lista de gateways
	pentru a retine ambele legaturi.
- addObstacle: Am verificat daca pozitia este valida si am adaugat obstacolul.


Miscari elemente:

- attemptMove: Pentru a misca un element, avem 3 variante:
	- Este BlankCell, de unde se desprind 2 cazuri
		-> Poate fi un Gateway care intre timp a fost suprascris de alt
		element si acum a devenit Blank. In acest caz, trebuie cautat
		in lista de Gateway-uri si returnata pozitia Gateway-ului pere-
		che (elementul s-a teleportat). Acest fapt se face cu ajutorul
		functiei findGatewayPair
		-> Chiar este Blank => returnam pozitia care este de tipul Maybe
		deci Just position
	- Este GatewayCell  -> returnam pozitia Gateway-ului pereche
	- Este ObstacleCell -> returnam Nothing deoarece nu exista nicio miscare
		valida.
- getMaybeValue: Functie ajutatoare care intoarce un Position dintr-un obiect de
	tip Maybe.
- goDirection: Functie generica pentru a misca Target-ul in functie de Behaviorul
	sau. Mai intai, verificam daca miscarea dictata de Behaviorul sau 
	conduce intr-o pozitie valida (lucru dictat de rezultatul functiei
	attemptMove : Nothing sau Just value). 
	Daca acest fapt este posibil, returnam un Target cu noua pozitie si 
	acelasi behaviour pe care l-am primit (adica goEast, goWest, goNorth sau
	goSouth).
	Daca miscarea nu poate fi facuta, verificam daca suntem pe un Gateway 
	sau nu pentru a vedea daca ne schimbam pozitia curenta. In mod asemana-
	tor, returnam un Target cu noua pozitie si acelasi behaviour.
- bounce: O alta functie care determina comportamentul unui Target.
	Daca pozitia dictata de behaviour-ul sau este valida, returnam Target-ul
	cu pozitia avansata si acelasi behavior.
	Altfel, schimbam behavior-ul (adica din North in South sau invers) si
	il miscam in directia noului behavior.
- moveTargets: Functie care misca Targeturile in functie de Behavior-ul lor.
	Pentru realizarea acestei functii, am urmat mai multi pasi.
	Am apelat functia deleteAllTargetsPositions pe Game-ul actual pentru a 
	sterge toate fostele pozitii ale Target-urilor. Informatiile despre 
	Target-uri nu sunt pierdute deoarece sunt pastrate in lista Targets din
	Game. Functia moveTargets primeste de 2 ori acelasi game, primul fiind
	cel care va fi modificat si returnat, iar al doilea va reprezenta o re-
	ferinta.
	Ne folosim de functia ajutatoare moveTargetsHelper. La fiecare iteratie
	se ia primul Target din lista targets si se retine noul Target obtinut
	prin aplicarea behavior-ului sau pe fostul Target (cu ajutorul functiei
	movementTypeTarget).
	Se sterg vechile Targets si din lista aplicand (tail targets) de fiecare
	data.
	Daca vechea pozitie era un Gateway, lista de cells ramane la fel.
	Altfel, doar inlocuim vechea pozitie cu Blank.
	Pe noile cell-uri modificate, adaugam noul Target folosind functia im-
	plementata inainte: addTarget (pe noua pozitie bineinteles).
	La final (cand al doilea game a epuizat lista de target-uri ce trebuie
	analizate), returnam primul game ce a suferit modificarile, caruia ii 
	aplicam functia reconstructGateways. Aceasta functie analizeaza toate
	Gateway-urile din list si repune simbolul de Gateway acolo unde a fost
	suprascris (adica daca era BlankCell).
- adjacentPositions1: Primeste 2 pozitii, a hunterului si a targetului. Verifica
	daca target-ul poate fi capturat, verificand daca pozitia targetului
	coincide cu vreuna dintre cele 4 pozitii adiacente ale Hunterului 
	(Nord, Sud, Est, Vest).
- isTargetKilled: Verifica daca un Target va fi omorat in functie de pozitia pe
	care o ocupa. Acest lucru se verifica cu functia definita mai sus,
	adjacentPositions1.


### Interactiune hunter si targets

- advanceGameState: Primeste directia in care dorim sa deplasam hunterul, un para-
	metru de tip Bool care dicteaza daca deplasam, respectiv omoram target
	urile sau nu si jocul.
	- Initial, trebuie sa aflam pozitia curenta a hunterului folosind 
	functia getCurrentPositionHunter care va primi numarul total de celule
	al jocului si jocul.
	- Deplasam Hunterul in directia data cu functia moveHunter. Aceasta
	functie analizeaza cele 4 directii pe care le poate urma hunterul, veri-
	fica daca sunt valide cu ajutorul functiei goHunter (un fel de 
	goDirection, doar ca aceasta serveste hunterul, nu targeturile) si 
	adauga noul Hunter pe pozitia corespunzatoare. Intoarce un nou joc in 
	care hunterul este actualizat.
	- Daca parametrul Bool este fals, returnam aceasta de joc deoarece nu
	trebuie sa interferam cu targeturile.
	- Altfel, omoram targeturile care au intrat in raza de omor a noii pozi-
	tii a targetului. Ne folosim de functia killTargets. Aceasta primeste
	jocul de 2 ori, asemanator cu moveTargetsHelper pentru a retine atat 
	noul joc, cat si o referinta a vechiului joc.
	Pentru fiecare Target din targets, verificam daca este omorat cu ajuto-
	rul functiei isTargetKilled. Daca da, il omoram cu ajutorul functiei
	killTargetAtPosition care primeste pozitia Targetului care trebuie 
	omorat. Ii inlocuieste celula cu un BlankCell si il sterge din targets
	cu functia updateTargetList.
	Altfel, analizam recursiv restul de targets din lista.
	- Continuand din noua stare a jocului, mutam restul de targets conform
	behaviorului lor folosind functia moveTargets
	- Omoram iar targeturile care au intrat in proximitatea hunterului.
	- Intoarcem jocul dupa aceste modificari asupra caruia ii aplicam 
	functia reconstructGateways 
- areTargetsLeft: intoarcem True daca lista de targets nu este vida. Altfel, fals.
- successors: primeste o stare a jocului, adica un game si calculeaza starile
	viitoare ale jocului, rezultate prin aplicarea miscarii in nord, sud,
	est, vest. Intoarce o lista de perechi intre directie si noul joc
- isGoal: Calculeaza pozitia curenta a hunterului si verifica daca poate omori
	primul Target din targets folosind functia isTargetKilled. Se face
	sau logic intre aceasta operatie si apelul recursiv al functiei pe
	restul de targets din joc.
	Astfel, functia intoarce True daca cel putin un Target poate fi 
	capturat.
- Bonus: circle: se foloseste de functia ajutatoare circleHelper care primeste
	un string ce reprezinta directia in care trebuie sa porneasca Targetul.
	Al doilea parametru este indexul din secventa pe care trebuie sa o par-
	curga. (O secventa reprezinta o secventa de miscari in aceaasi directie.
	Ordinea (circulara) si paleta de miscari realizate de target:
		- goEast -> goSouthEast -> goSouth -> goSouthWest -> goWest
			-> goNorthWest -> noNorh -> goNorthEast -> goEast)
	Al treilea parametru reprezinta numarul de pasi care trebuie executati
	pentru o miscare tip linie: pe orizontala sau verticala (adica inspre 
	unul dintre cele 4 puncte cardinale).
	Al patrulea parametru reprezinta numarul de pasi care trebuie executati
	pentru o miscare tip diagonala: pentru miscari combinate.

	Directia de start, indexul secventei si cele 2 numere trebuie calculate 
	in circle in functie de punctul de start al Targetului, centru si raza 
	cercului.

## Search.hs

Am definit tipul de data Node s a avand campurile conforme cu cele din comen-
tarii. Pentru stabilirea tipului pentru fiecare camp, m-am uitat la semnatura
getterilor de mai jos.

Instantierea de Eq si Ord se face in functie de starea jocului.
Getterii au fost realizati prin pattern matching pentru a intoarce campul
corespunzator.

- createStateSpace: Avem nevoie de un helper care sa genereze recursiv nodurile 
	din arbore. Am apelat aceasta functie cu nodul root - initialState,
	actiunea (Nothing), parintele (Nothing) si depth-ul (0).
- createStateSpaceHelper: returneaza un nod pe care il construieste cu datele pri-
	mite ca parametru. Pentru euristica, folosim apelam h pe state pentru a
	afla costul. Pentru generarea copiilor, trebuie sa apelam functia recur-
	siv pentu a putea crea nodurile copil).
	Starile urmatoarea sunt generate folosind functia successors din Basics.
	Aceasta ne intoarce o lista de perechi (actiune, stare), iar functia
	createSpaceHelper trebuie apelata pe fiecare element in parte. Astfel,
	am folosit functionala map.
- suitableSuccs: Accesam nodurile succesor, folosind getterul nodeChildren.
	Trebuie sa alegem din rezultatul intors nodurile care nu au fost vizita-
	te, deci care nu apartin visited. Facem filter cu functia notMember
	pentru a afla noua lista de noduri nevizitate.
- insertSucc: Inseram nodul (node) in coada de prioritati (frontier). Functia pe
	care i-o dam ca parametru lui insertWith va fi functia min deoarece 
	mereu cautam drumul cel mai scurt. Apoi, adaugam nodul si prioritatea
	care este calculata ca suma dintre adancime si euristica. Deoarece au 
	tipuri diferite, trebuie sa convertim euristica folosind functia 
	fromIntegral pentru a converti din int la tipul numeric.
- insertSuccs: Trebuie sa extragem nodurile succesor valide, deci apelam functia
	suitableSuccs pe node. Rezultatul intors trebuie adaugat in coada de 
	prioritati. Pentru a evita recursivitatea, folosim foldl cu functia
	insertSucc pentru a adauga fiecare element pe rand in coada frontier.
- astar': Extragem nodul cu prioritatea minima folosind functia deleteFindMin.
	Verificam daca nodul corespunde unei stari scop (stare scop <-> o stare
	in care un Hunter poate fi captura un Target). Daca da, il returnam.
	Daca nu, ii marcam starea ca fiind vizitata, adaugandu-l la setul 
	visited si inserandu-i succesorii in frontiera (folosindu-ne de functia
	insertSuccs implementata inainte). Succesorii vor fi adaugati in coada 
	returnata de functia deleteFindMin.
- astar: Functie care apeleaza astar'. Primul parametru al lui astar' este un set
	vid, iar al doilea o noua coada de prioritati in care este inserat nodul
	initial. Si pentru acesta, trebuie calculat costul ca suma dintre 
	adancime si euristica.
- extractPath: Am urmat modelul din laboratorul 2. Daca exista un nod parinte,
	apelam functia recursiv si concatenam cu perechea starii curente de tip
	(actiune, stare).
