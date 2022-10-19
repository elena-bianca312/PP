:- ensure_loaded('checker.pl').

%test_mode(detailed).

% Considerăm următoarele reprezentări:
%
% O integramă este reprezentată prin structura (compusul)
% integ(H, W, Lista, Vocab), unde:
% H este înălțimea integramei
% W este lățimea integramei
% Lista este o listă de tupluri (Poz, Valoare), unde
%   Poz este un tuplu (R, C) conținând rândul și coloana (0-based)
%   Valoare este una dintre:
%     x - dacă celula este neagră (nu poate fi completată cu litere)
%     o literă, dacă celula este completată cu o literă
%     o listă de întrebări, reprezentate ca tupluri (Text, Dir, ID), cu
%       Text - un srting, textul întrebării
%       Dir - una dintre valorile j sau d, indicând direcția întrebării
%       ID - un identificator numeric al întrebării
% Vocab este o listă de stringuri reprezentând cuvinte disponibile
% pentru a rezolva întrebarea.
%
% În ieșirea predicatului intrebări, o întrebare este reprezentată ca
% ((R, C), Text, Dir, ID), unde
% R este rândul căsuței cu întrebarea (0-based)
% C este coloana căsuței cu întrebarea (0-based)
% Text este textul întrebării (un string)
% Dir este j sau d, reprezentând direcția în care trebuie să fie plasat
% răspunsul (jos sau dreapta)
% ID este un identificator numeric al întrebării.

% Puteți vizualiza integramele cu:
% integrama(0, W), print_integrama(W).
% integrama(1, W), print_integrama(W).
% integrama(2, W), print_integrama(W).
% integrama(3, W), print_integrama(W).
%
% Testați cu
% vmtest.
% Testați teste individuale (vedeți predicatul tt din checker.pl) cu
% vmtest(Test).
% de exemplu cu vmtest(intrebari).


% intrebari/2
% intrebari(integ(+H, +W, +Lista, +Vocab), -Lista_intrebari)
% Este adevărat atunci când Lista_intrebari este o lista de tupluri
% ((R, C), Text, Dir, ID), fiecare tuplu corespunzând unei întrebări din
% integramă (rândul, coloana, textul întrebării, direcția (j/d),
% identificatorul).
% BONUS: intrebari are o singură soluție (o singură listă) pentru o
% anumită integramă. 

intrebari2(integ(_, _, [], _), []) :- !.
intrebari2(integ(H, W, [((R, C), [(Text, Dir, ID)])|Rest1], _), [((R, C), (Text, Dir, ID))|Rest2]) :- 
    intrebari2(integ(H, W, Rest1, _), Rest2).
intrebari2(integ(H, W, [((R, C), [(Text1, Dir1, ID1), (Text2, Dir2, ID2)])|Rest1], _), 
[((R, C), (Text1, Dir1, ID1)), ((R, C), (Text2, Dir2, ID2))|Rest2]) :-
    intrebari2(integ(H, W, Rest1, _), Rest2).
intrebari2(integ(H, W, [_|Rest1], _), List) :- intrebari2(integ(H, W, Rest1, _), List).

intrebari(integ(H, W, List, Vocab), L) :- once(intrebari2(integ(H, W, List, Vocab), L)).


% id_intrebare/2
% id_intrebare(+Integ, ?Intrebare, ?Q_ID)
% Este adevărat dacă în integrama reprezentată ca integ(...), Intrebare
% este un text iar Q_ID este un identificator care corespund aceleași
% întrebări.

id_intrebare(integ(H, L, List, Vocab), Intrebare, Q_ID) :- 
    intrebari(integ(H, L, List, Vocab), X), member(((_, _), Intrebare, _, Q_ID), X), !.

% completare/3
% completare(+Integ, +Sol, -Integrama)
% Predicatul produce Integrama, o structură de forma integ(...),
% pornind de la Integ, în care au fost completate celule conform cu
% soluția Sol.
% Soluția este reprezentată ca o listă de perechi (Întrebare, Răspuns),
% unde Întrebarea este textul unei întrebări, iar Răspuns este un cuvând
% de completat; ambele sunt stringuri.
% De exemplu, o soluție parțială pentru integrama 0 poate fi:
% [('Din care plouă', 'NOR'), ('Al doilea număr', 'DOI')]
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), solutie(0, Sol), completare(W, Sol, W2),
%   print_integrama(W2).

% sort -> to remove duplicates
completare_helper(integ(H, L, List, Vocab), (_, _), _, [], integ(H, L, List2, Vocab)) :- sort(List, List2), !.

% complete vertically
completare_helper(integ(H, L, List, Vocab), (Cx, Cy), Dir, [Lett|Letters], integ(H, L, List2, Vocab)) :-
    Dir == j, !,
    Cx1 is Cx + 1,
    completare_helper(integ(H, L, [((Cx1, Cy), Lett)|List], Vocab), (Cx1, Cy), Dir, Letters, integ(H, L, List2, Vocab)).

% complete horizontallt
completare_helper(integ(H, L, List, Vocab), (Cx, Cy), Dir, [Lett|Letters], integ(H, L, List2, Vocab)) :-
    Dir == d, !,
    Cy1 is Cy + 1,
    completare_helper(integ(H, L, [((Cx, Cy1), Lett)|List], Vocab), (Cx, Cy1), Dir, Letters, integ(H, L, List2, Vocab)).

completare(integ(H, L, List, Vocab), [], integ(H, L, List2, Vocab)) :- sort(List, List2), !.
completare(integ(H, L, List, Vocab), [(Text, Ans)|Sol], integ(H, L, List2, Vocab)) :-
    atom_chars(Ans, Letters),
    intrebari(integ(H, L, List, Vocab), X), member(((Cx, Cy), Text, Dir, _), X),
    completare_helper(integ(H, L, List, Vocab), (Cx, Cy), Dir, Letters, integ(H, L, List3, Vocab)),
    completare(integ(H, L, List3, Vocab), Sol, integ(H, L, List2, Vocab)).

% lungime_spatiu/3
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), +Intrebare, -Lungime)
% Returnează lungimea spațiului asociat întrebării date.
% Întrebarea este indicată prin textul ei. De exemplu:
% lungime_spatiu pentru integrama 0 și întrebarea 'Al doilea număr'
% trebuie să lege Lungime la 3.
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), id_intrebare(W, Text, 3), lungime_spatiu(W, Text, X).
lungime_spatiu(_, _, _) :- false.

% intersectie/5
% intersectie(integ(+H, +W, +Lista, +Voc), +I1, -Poz1, +I2, -Poz2)
% Pentru o integramă și două întrebări date prin textul lor (I1 și I2),
% al căror răspunsuri se intersectează, întoarce în Poz1 indicele din
% răspunsul la I1 la care este intersecția, și în Poz2 indicele din
% răspunsul la I2 la care este intersecția. Indecșii incep de la 0.
%
% De exemplu, în integrama 0:
%  █       █       2↓      3↓      █
%  █       0↓,1→   -       -       █
%  4→      -       -       -       █
%  5→      -       -       -       █
%  █       █       █       █       █
%
%  Întrebările 'Primii 3 din artă' și 'Afirmativ' (3, respectiv 1) se
%  intersectează la pozițiile 0, respectiv 2 (va fi litera A, de la
%  ART, respectiv DA).
intersectie(_, _, _, _, _) :- false.

% solutii_posibile/2
% solutii_posibile(integ(+H, +W, +Lista, +Vocabular), -Solutii)
% Formează o listă Solutii, conținând perechi de forma
% (Întrebare, Cuvinte), unde
% Întrebare este textul unei întrebări din integramă, iar Cuvinte este o
% listă de cuvinte sunt din Vocabular și au lungimea corectă pentru a fi
% răspuns la întrebare. Solutii conține câte o pereche pentru fiecare
% întrebare din integramă.
% Cuvintele sunt reprezentate ca liste de stringuri, fiecare string
% având lungime 1 (o singură literă).
% De exemplu, pentru integrama 0, Solutii conține 6 perechi, două dintre
% ele fiind:
% ('Afirmativ', [['D', 'A'], ['N', 'U']])
% ('Din care plouă',
% [['N','O','R'],['A','R','T'],['U','I','T'],['D','O','I']])
solutii_posibile(_, _) :- false.

% rezolvare/2
% rezolvare(+Integ, -Solutie)
% Rezolvare produce în Solutie soluția integramei Integ. Soluția este
% reprezentată ca o listă de perechi de stringuri, fiecare pereche
% conținând textul unei întrebări și cuvântul (ca string) care este
% răspunsul la întrebare.
rezolvare(_, _) :- false.
