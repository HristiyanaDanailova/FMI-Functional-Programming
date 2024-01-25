#lang r5rs
(#%require "../commonFunctions.rkt")
(#%require racket/port)
;opens a port to read the input file from
(define in (open-input-file "input2.txt"))

;helper functions returning a row, a column or an element
(define (get-row i m) (list-ref m i))
(define (get-col i m) (map (lambda(x)(list-ref x i)) m))
(define (get-el i j m) (get-row i (get-col j m)))

;returns the elements of the list that цоулд hide the given element 'el'
(define (taller-than el l)
  (filter (lambda(x) (char>=? x el)) l))

;returns a list of 4 sublists with elements - the elements on the same row after and before the element, and the elements in the same column above and below the element 
(define (make-lsts i j m)
 (list (sublist (get-row i m) 0 (- j 1))
       (sublist (get-row i m) (+ j 1) (length (car m)))
       (sublist (get-col j m) 0 (- i 1))
       (sublist (get-col j m) (+ i 1) (length (get-col j m)))
       ) )
;filters the elements that could hide the given element for every list returned by (make-lsts) and if the returned lists are empty then the tree is hidden, else is visible
(define (visible? i j m)
 (if (null? (filter null? (map (lambda(y)(taller-than (get-el i j m) y)) (make-lsts i j m))))
     #f
     #t))

;generates a list with elements from 1 to length
(define (gen-it length)
  (define (helper a b res)
    (if (> a b)
        res
        (helper (+ a 1) b (append res (list a)))))
  (helper 1 length '()))

;maps the values #t/#f to every element based on its visability
(define (walk i m)
  (map (lambda(x)(visible? i x m)) (gen-it (- (length (car m)) 2))))

;for every row of the matrix (except the inner ones) is applied the function 'walk'; the returned lists consist of values #t/#f
(define (match m)
  (define (helper curr res)
    (if ( = curr ( - (length m) 1))
        res
        (helper (+ curr 1) (append res (walk curr m)))))
  (helper 1 '()))

;returnes the amount of the outter elements for the matrix
(define (outter m)
  (+ (* 2 (length (car m)))
     (* 2 (- (length m) 2))))

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

;finds the total amount of visible trees
(display (+ (outter mat)(length (filter (lambda(x)(eq? x #t)) (match mat)))))

(close-input-port in)
