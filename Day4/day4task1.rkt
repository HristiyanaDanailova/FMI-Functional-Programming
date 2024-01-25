#lang r5rs
(#%require "../commonFunctions.rkt")
(#%require racket/port)
;opens a port to read the input file from
(define in (open-input-file "input2.txt"))

;splits the input list into a list of pairs, where the elements of each pair get splitted, using comma as a delimiter 
(define (make-pairs l)
  (if (null? l) l
      (append (list (cons (car (string-split char-comma? (car l)))
                          (car (cdr (string-split char-comma? (car l)))))) (make-pairs (cdr l)))
              ))

;checks if one pair contains the other
(define (valPair? fPair sPair)
  (if (or (and (>= (car fPair) (car sPair))
           (<= (cdr fPair) (cdr sPair))
           )
          (and (>= (car sPair) (car fPair))
           (<= (cdr sPair) (cdr fPair))
           )
       )
      #f
      #t
   ))

;reads the input file
(define inp (port->string in))
;-> "48-48,48-88\r\n59-92,80-95\r\n82-87,6-83\r\n81-97,13-81\r\n13-75,14-75\r\n97-98,21-97\r\n14-93,13-13\r\n21-32,20-31\r\n30-71,72-72"

;splits the input string into list of strings using the new line character as a delimiter
(define inpToList (string-split char-whitespace? inp))
;-> ("48-48,48-88" "59-92,80-95" "82-87,6-83" "81-97,13-81" "13-75,14-75" "97-98,21-97" "14-93,13-13" "21-32,20-31" "30-71,72-72")

;splits the list of string values into a list of pairs
(define pairs (make-pairs inpToList))
;-> (("48-48" . "48-88")
;   ("59-92" . "80-95")
;   ("82-87" . "6-83")
;   ("81-97" . "13-81")
;   ("13-75" . "14-75")
;   ("97-98" . "21-97")
;   ("14-93" . "13-13")
;   ("21-32" . "20-31")
;   ("30-71" . "72-72"))

;splits the list of pairs into a list of 2-pair element sublists 
(define sepPairs
  (map (lambda(x) (list (cons (string->number (car (string-split char-dash? (car x)))) (string->number (car (cdr (string-split char-dash? (car x))))))
                        (cons (string->number (car (string-split char-dash? (cdr x)))) (string->number (car (cdr (string-split char-dash? (cdr x)))))))) pairs))
;-> (((48 . 48) (48 . 88))
;   ((59 . 92) (80 . 95))
;   ((82 . 87) (6 . 83))
;   ((81 . 97) (13 . 81))
;   ((13 . 75) (14 . 75))
;   ((97 . 98) (21 . 97))
;   ((14 . 93) (13 . 13))
;   ((21 . 32) (20 . 31))
;   ((30 . 71) (72 . 72)))

;filters the pairs, where one of the elements contains the other
(define invalPairs
  (filter (lambda(x) (eq? #f (valPair? (car x) (car (cdr x))))) sepPairs))
;-> (((48 . 48) (48 . 88)) ((13 . 75) (14 . 75)))

;returns the number of pairs, where one of the elements contains the other
(display (length invalPairs))

(close-input-port in)