#lang r5rs
(#%require "../commonFunctions.rkt")
(#%require racket/port)
;opens a port to read the input file from
(define in (open-input-file "input2.txt"))

;helper functions returning a row, a column or an element
(define (get-row i m) (list-ref m i))
(define (get-col i m) (map (lambda(x)(list-ref x i)) m))
(define (get-el i j m) (get-row i (get-col j m)))

;creates a list of 4 sublists with elements - the elements on the same row after and before the element, and the elements in the same column above and below the element 
;revrses the order of the of elements before the given elements in the rows and columns
(define (make-lsts i j m)
 (list (reverse (sublist (get-row i m) 0 (- j 1)))
       (sublist (get-row i m) (+ j 1) (length (car m)))
       (reverse (sublist (get-col j m) 0 (- i 1)))
       (sublist (get-col j m) (+ i 1) (length (get-col j m)))
       ) )

;generates a list with elements from 0 to length
(define (gen-it length)
  (define (helper a b res)
    (if (> a b)
        res
        (helper (+ a 1) b (append res (list a)))))
  (helper 0 length '()))

;cuts the list to the first element that hides the view
(define (check el l)
  (if (null? l)
      '()
      (if (char<? (car l) el)
                (cons (car l) (check el (cdr l)))
                (cons (car l) '())
                   )))
;cuts every list returned by the function make-lsts, using the function 'check' and maps its length to it 
;реже всеки лист на съседтсво с check и му map-ва дължината
(define (visibleList i j m)
 (map length (map (lambda(x) (check (get-el i j m) x))(make-lsts i j m))))

;returns the scenery score for every element on row i
(define (scores-row i m)
  (map (lambda(y)(foldr * 1 y))(map (lambda(x)(visibleList i x m)) (gen-it (- (length (car m)) 1)))))
 
; matches the scenery score list to every row of m
(define (match m)
  (define (helper curr res)
    (if (= curr ( - (length m) 1))
        res
        (helper (+ curr 1) (append res (scores-row curr m)))))
  (helper 0 '()))

;reads the input
(define input (port->string in))
;-> "30373\r\n25512\r\n65332\r\n33549\r\n35390"

;splits the input string into a list of strings, using whitespace as a delimiter
(define splitInp (string-split char-whitespace? input))
;-> ("30373" "25512" "65332" "33549" "35390")

;splits every string element of the list into a sublist of characters
(define mat (map string->list splitInp))
;-> ((#\3 #\0 #\3 #\7 #\3)
;   (#\2 #\5 #\5 #\1 #\2)
;   (#\6 #\5 #\3 #\3 #\2)
;   (#\3 #\3 #\5 #\4 #\9)
;   (#\3 #\5 #\3 #\9 #\0))

;finds the tree with the highest scenery score
(display (foldr max 0 (match mat)))

(close-input-port in)
