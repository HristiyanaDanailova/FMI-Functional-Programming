#lang r5rs
(#%require "../commonFunctions.rkt")
(#%require racket/port)
;opens a port to read the input file from
(define in (open-input-file "input2.txt"))

;splits a list into sublists by a given delimiter
;https://codereview.stackexchange.com/questions/87058/splitting-a-list-in-racket
(define (split-by lst x)
  (foldr (lambda (element next)
           (if (eq? element x)
               (cons '() next)
               (cons (cons element (car next)) (cdr next))))
         (list '()) lst))

;helper functions for accessing the first,second and third element of a list
(define fst car)
(define scd cadr)
(define trd caddr)

;if el is an element of the list l, its position gets returned
;else the number -1 is returned
(define (get-list-index l el)
    (if (null? l)
        -1
        (if (equal? (car l) el)
            0
            (let ((result (get-list-index (cdr l) el)))
                (if (= result -1)
                    -1
                    (+ 1 result))))))

;inserts the element 'item' iat position n in list 'list'
(define (insert-n list item n)
  (if (= n 0)
      (cons item list)
      (cons (car list) (insert-n (cdr list) item (- n 1)))))

;constructs a list of pairs of type: (element of l . its position in l)
(define (constr l)
  (define (helper curr res)
    (if (= curr (length l))
        res
        (helper (+ curr 1) (append res (list (cons (list-ref l curr) curr))))))
  (helper 0 '()))

;deletes 'item' from 'list'
(define (delete item list) (filter (lambda (x) (not (equal? x item))) list))

;calculates the new position of the element in l, by applying the modulo operation, simulating a cyclic list
(define (position el l)
  (let ((newInd (modulo (+ (-(length l)1) (modulo (+ (get-list-index l el) (fst el)) (-(length l)1))) (- (length l)1))))
       (if (or (> newInd (- (length l) 1))
               (< newInd 0))
           (modulo newInd (- (length l) 1))
            (if (= newInd 0)
                (-(length l)1)
                newInd))))

;moves an element from its old index 'oldInd' to a new one
(define (move el oldIn l)
  (let ((newl (delete el l))
       (newp (position el l)))
    (insert-n  newl el newp)
  
  ))

;finds every element of l and moves it to a new position, calculated with the function 'move'
(define (mix l)
  (define (helper res ls curr)
    (if (= curr (length l))
        res     
        (helper (move (cons (car ls) curr) (get-list-index res (cons (car ls) curr)) res) (cdr ls) (+ curr 1))))
  (helper (constr l) l 0))

;finds the position of 0 and then sums the 1000-th,2000-th and 3000-th element after 0 and sums them
(define (sumIt r l)
  (let ((zero (get-list-index r (car (filter (lambda(x) (= (car x) 0)) r))
              )))
    (+ (fst (list-ref r (modulo (+ 1000 zero) (length r))))
       (fst(list-ref r (modulo (+ 2000 zero) (length r))))
       (fst(list-ref r (modulo (+ 3000 zero) (length r))))
     )
      ))

;reads the input
(define inp (port->string in))
;-> "1\r\n2\r\n-3\r\n3\r\n-2\r\n0\r\n4"

;splits the string into a list of chracters
(define input (string->list inp))
;-> (#\1 #\return #\newline #\2 #\return #\newline #\- #\3 #\return #\newline #\3 #\return #\newline #\- #\2 #\return #\newline #\0 #\return #\newline #\4)

;leaves in the number characters and transforms them from a char to a number
(define nums (map string->number (map (lambda(z) (list->string z))(map (lambda(x) (filter (lambda(y) (not (char=? y #\return))) x))(split-by input #\newline)))))
;->(1 2 -3 3 -2 0 4)

;transforms a list of elements to a list ot pairs of type: (element index)
(define con (constr nums))
;-> ((1 . 0) (2 . 1) (-3 . 2) (3 . 3) (-2 . 4) (0 . 5) (4 . 6))

;sums the three numbers that form the grove coordinates
(display (sumIt (mix nums) nums))


(close-input-port in)