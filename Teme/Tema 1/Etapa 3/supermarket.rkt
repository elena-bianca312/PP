#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (define C (make-counter index 0 0 empty-queue)) C)


; UPDATE
(define (update f counters index)
  (map (λ (C) (if (= (counter-index C) index) (f C) C)) counters))

(define (update-neg f counters index)
  (map (λ (C) (if (not (= (counter-index C) index)) (f C) C)) counters))


; TT+
(define (tt+ minutes)
  (λ (C)
    (struct-copy counter C [tt (+ minutes (counter-tt C))])))


; ET+
(define (et+ minutes)
  (λ (C)
    (struct-copy counter C [et (+ minutes (counter-et C))])))


; ADD-TO-COUNTER
(define (add-to-counter name items)
  (λ (C)
    (struct-copy counter C
                 [tt (+ items (counter-tt C))]
                 [et (+ (if (queue-empty? (counter-queue C)) items 0) (counter-et C))]
                 [queue (enqueue (cons name items) (counter-queue C))])))


; MIN-HELPER
(define (min-param f counters)
  (if (null? counters)
      counters
      (if (= (length counters) 1)
          (cons (counter-index (car counters)) (f (car counters)))
          (if (<= (f (car counters)) (f (cadr counters)))
              (min-param f (append (list (car counters)) (cddr counters)))
              (min-param f (cdr counters))))))

        
; MIN-TT + MIN-ET
(define (min-tt counters) (min-param (λ(C) (counter-tt C)) counters)) 
(define (min-et counters) (min-param (λ(C) (counter-et C)) counters))


; REMOVE-FIRST-FROM-COUNTER
(define (remove-first-from-counter C)
  (struct-copy counter C
               [tt (if (queue-empty? (counter-queue C)) 0 (- (counter-tt C) (counter-et C)))]
               [et (if (queue-empty? (dequeue (counter-queue C))) 0 (cdr (top (dequeue (counter-queue C)))))]
               [queue (if (queue-empty? (counter-queue C)) empty-queue (dequeue (counter-queue C)))]))


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!

; PASS-TIME-THROUGH-COUNTER
(define (pass-time-through-counter minutes)
  (λ (C)
    (struct-copy counter C
                 [tt (if (< (- (counter-tt C) minutes) 0) 0 (- (counter-tt C) minutes))]
                 [et (if (< (- (counter-et C) minutes) 0) 0 (- (counter-et C) minutes))])))



; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.

; FIND-AVERAGE
(define (find-average number acc counters)
  (if (null? counters)
      (if (= number 0)
          0
          (/ acc number))
      (find-average (+ number 1) (+ acc (counter-tt (car counters))) (cdr counters))))


; ENSURE-HELPER
(define (ensure-helper fast-counters slow-counters average)
  (if (<= (find-average 0 0 (append fast-counters slow-counters)) average)
      slow-counters
      (ensure-helper fast-counters (append slow-counters (list (empty-counter (+ (length slow-counters) (length fast-counters) 1)))) average)))


; FIND-COUNTER-BY-INDEX
(define (find-counter-by-index counters index)
  (if (null? counters)
      counters
      (if (= (counter-index (car counters)) index)
          (car counters)
          (find-counter-by-index (cdr counters) index))))


; PASS-TIME-HELPER
(define (pass-time-helper minutes requests fast-counters slow-counters acc)
  
  (define next-client (min-et (filter (λ (C) (not (queue-empty? (counter-queue C)))) (append fast-counters slow-counters))))
  (if (null? requests)
      (cons acc (append fast-counters slow-counters))
      (if (not (null? next-client))
          (if (> (cdr next-client) minutes)
              (serve-helper (cdr requests)
                            (map (pass-time-through-counter minutes) fast-counters) (map (pass-time-through-counter minutes) slow-counters) acc)
              (pass-time-helper (- minutes (cdr next-client)) requests
                                (update-neg (pass-time-through-counter (cdr next-client))
                                            (update remove-first-from-counter fast-counters (car next-client)) (car next-client))
                                (update-neg (pass-time-through-counter (cdr next-client))
                                            (update remove-first-from-counter slow-counters (car next-client)) (car next-client))
                                (if (null? acc)
                                    (list (cons (car next-client)
                                                (car (top (counter-queue (find-counter-by-index (append fast-counters slow-counters) (car next-client)))))))
                                    (append acc (list (cons (car next-client)
                                                            (car (top (counter-queue (find-counter-by-index (append fast-counters slow-counters) (car next-client)))))))))))
          (serve-helper (cdr requests)
                        (map (pass-time-through-counter minutes) fast-counters) (map (pass-time-through-counter minutes) slow-counters) acc))))

(define C1 (empty-counter 1))
(define C2 (empty-counter 2))
(define C3 (empty-counter 3))
(define C4 (empty-counter 4))
(define C5 (make-counter 5 12 8 (queue '((remus . 6) (vivi . 4)) '() 2 0)))


; SERVE-HELPER
(define (serve-helper requests fast-counters slow-counters acc)

  (if (null? requests)
      (cons acc (append fast-counters slow-counters))
      (match (car requests)

        [(list 'ensure average)
         (serve-helper (cdr requests) fast-counters (ensure-helper fast-counters slow-counters average) acc)]

;        [(list name n-items)
;         (if (or (< (cdr (min-tt slow-counters)) (cdr (min-tt fast-counters))) (> n-items ITEMS))
;             (serve-helper (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))) acc)
;             (serve-helper (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt fast-counters))) slow-counters acc))]

        [(list name n-items)
         (if (> n-items ITEMS)
             (serve-helper (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))) acc)
             (serve-helper (cdr requests)
                           (update (add-to-counter name n-items) fast-counters (car (min-tt (append fast-counters slow-counters))))
                           (update (add-to-counter name n-items) slow-counters (car (min-tt (append fast-counters slow-counters))))
                           acc))]
        
        [(list 'delay index minutes)
         (if (<= index (length fast-counters))
             (serve-helper (cdr requests) (update (tt+ minutes) (update (et+ minutes) fast-counters index) index) slow-counters acc)
             (serve-helper (cdr requests) fast-counters (update (tt+ minutes) (update (et+ minutes) slow-counters index) index) acc))]
        
        [else (pass-time-helper (car requests) requests fast-counters slow-counters acc)])))

; SERVE
(define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters slow-counters '()))
  
