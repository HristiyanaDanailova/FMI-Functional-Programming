#lang r5rs
(#%require "../commonFunctions.rkt")
(#%require racket/port)
;opens a port to read the input file from
(define in (open-input-file "input2.txt"))

;checks if l contains the element x
(define (memberR x l)
  (if (null? l)
      #f
      (if (eq? x (car l))
          #t
          (memberR x (cdr l)))))

;checks if x is an unique element for the list l
(define (unique? x l)
  (if (null? l)
      #t
      (if (eq? x (car l))
          (if (memberR (car l) (cdr l))
              #f
               #t)
          (unique? x (cdr l)))))

;checks if every element of the list l is unique and if there are no repetitions
(define (marker? l)
 (if (null? (filter (lambda(y) (eq? y #f))(map (lambda(x) (unique? x l)) l )))
     #t
     #f))

;creates a 14-element list for every element of l and if it is a marker,it returns the index
(define (marker l)
  (define (helper ls ind res)
    (if (or (= ind (- (length l) 13))
            (not (= res -1)))
        res
        (helper (cdr ls) (+ ind 1)
                (if (marker? (sublist l ind (+ 13 ind)))
                    (+ 14 ind)
                    res)
                )))
  (helper l 0 -1))

;reads the input
(define input (port->string in))
;-> "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

;splits the input string into a list of chars
(define inputToLst (string->list input))
;-> (#\m #\j #\q #\j #\p #\q #\m #\g #\b #\l #\j #\s #\p #\h #\d #\z #\t #\n #\v #\j #\f #\q #\w #\r #\c #\g #\s #\m #\l #\b)

;the number of characters before the first start-of-message marker 
(display (marker inputToLst))

(close-input-port in)


