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

;generates a list containing the values of the interval a-b
(define (interval a b)
  (if (> a b)
      '()
      (cons a (interval (+ a 1) b))))

;checks if l contains the element x
(define (memberR x l)
  (if (null? l)
      #f
      (if (eq? x (car l))
          #t
          (memberR x (cdr l)))))

;filters the elements which are simultaneously contained in list1 and list2, and if the filtered list is empty, then the pairs are valid
;(if fPair=(fP1 fP2), then list1 is the list returned by (interval fP1 fP2)
;(if sPair=(sP1 sP2), then list2 is the list returned by (interval sP1 sP2)
(define (valPair? fPair sPair)
  (if (null? (filter (lambda(x) (memberR x (interval (car sPair) (cdr sPair)))
            )  (interval (car fPair) (cdr fPair))))
      #t
      #f))


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

;filters the overlaping pairs
(define invalPairs
  (filter (lambda(x) (eq? #f (valPair? (car x) (car (cdr x))))) sepPairs)
)
;-> (((48 . 48) (48 . 88)) ((59 . 92) (80 . 95)) ((82 . 87) (6 . 83)) ((81 . 97) (13 . 81)) ((13 . 75) (14 . 75)) ((97 . 98) (21 . 97)) ((21 . 32) (20 . 31)))

;returns the number of pairs which overlap
(display (length invalPairs))
(close-input-port in)