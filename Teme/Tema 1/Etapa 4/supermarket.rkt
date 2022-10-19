#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue

(define-struct counter (index tt et queue closed) #:transparent)

(define (empty-counter index)
  (make-counter index 0 0 empty-queue 0))

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

; PASS-TIME-THROUGH-COUNTER
(define (pass-time-through-counter minutes)
  (λ (C)
    (struct-copy counter C
                 [tt (if (<= (- (counter-tt C) minutes) 0) 0 (- (counter-tt C) minutes))]
                 [et (if (<= (- (counter-et C) minutes) 0) 0 (- (counter-et C) minutes))])))
  
  
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei

; FIND-AVERAGE
(define (find-average number acc counters)
  (if (null? counters)
      (if (= number 0)
          0
          (/ acc number))
      (find-average (+ number 1) (+ acc (counter-tt (car counters))) (cdr counters))))


; ENSURE-HELPER
(define (ensure-helper fast-counters slow-counters average)
  (if (<= (find-average 0 0 (filter (λ (C) (equal? (counter-closed C) 0)) (append fast-counters slow-counters))) average)
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
(define (pass-time-helper minutes requests acc fast-counters slow-counters)
  
  (let* ((next-client (min-et (filter (λ (C) (not (queue-empty? (counter-queue C)))) (append fast-counters slow-counters))))
        (C (find-counter-by-index (append fast-counters slow-counters) (car next-client))))
    (if (null? requests)
        (cons acc (print-function (filter (λ (C) (not (queue-empty? (counter-queue C)))) (append fast-counters slow-counters)) '()))
        (if (and (not (null? next-client)) (not (queue-empty? (counter-queue C))))
            (if (> (cdr next-client) minutes)
                (serve-helper (cdr requests) 
                              (map (pass-time-through-counter minutes) fast-counters) (map (pass-time-through-counter minutes) slow-counters) acc)
                (pass-time-helper (- minutes (cdr next-client)) requests
                                  (if (null? acc)
                                      (list (cons (car next-client)
                                                  (car (top (counter-queue (find-counter-by-index (append fast-counters slow-counters) (car next-client)))))))
                                      (append acc (list (cons (car next-client)
                                                              (car (top (counter-queue C)))))))
                                  (update-neg (pass-time-through-counter (cdr next-client))
                                              (update remove-first-from-counter fast-counters (car next-client)) (car next-client))
                                  (update-neg (pass-time-through-counter (cdr next-client))
                                              (update remove-first-from-counter slow-counters (car next-client)) (car next-client))))
            (serve-helper (cdr requests)
                          (map (pass-time-through-counter minutes) fast-counters) (map (pass-time-through-counter minutes) slow-counters) acc)))))

(define (print-function counters acc)
  (if (null? counters)
      (reverse acc)
      (print-function (cdr counters) (append (list (cons (counter-index (car counters)) (counter-queue (car counters)))) acc))))


; SERVE-HELPER
(define (serve-helper requests fast-counters slow-counters acc)

  (if (null? requests)
      (cons acc (print-function (filter (λ (C) (not (queue-empty? (counter-queue C)))) (append fast-counters slow-counters)) '()))
      (match (car requests)

        [(list 'close index)
         (serve-helper (cdr requests)
                       (update (λ (C) (struct-copy counter C [closed 1])) fast-counters index)
                       (update (λ (C) (struct-copy counter C [closed 1])) slow-counters index)
                       acc)]

        [(list 'ensure average)
         (serve-helper (cdr requests) fast-counters (ensure-helper fast-counters slow-counters average) acc)]

        [(list name n-items)
         (let [(slow-counters-filtered (filter (λ (C) (equal? (counter-closed C) 0)) slow-counters))
               (fast-counters-filtered (filter (λ (C) (equal? (counter-closed C) 0)) fast-counters))]
           (if (> n-items ITEMS)
               (serve-helper (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters-filtered))) acc)
               (serve-helper (cdr requests)
                             (update (add-to-counter name n-items) fast-counters (car (min-tt (append fast-counters-filtered slow-counters-filtered))))
                             (update (add-to-counter name n-items) slow-counters (car (min-tt (append fast-counters-filtered slow-counters-filtered))))
                             acc)))]
        
        [(list 'delay index minutes)
         (if (<= index (length fast-counters))
             (serve-helper (cdr requests) (update (tt+ minutes) (update (et+ minutes) fast-counters index) index) slow-counters acc)
             (serve-helper (cdr requests) fast-counters (update (tt+ minutes) (update (et+ minutes) slow-counters index) index) acc))]
        
        [else (pass-time-helper (car requests) requests acc fast-counters slow-counters)])))

; SERVE
(define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters slow-counters '()))