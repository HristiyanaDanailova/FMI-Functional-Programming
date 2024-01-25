#lang r5rs
(#%require "../commonFunctions.rkt")
(#%require racket/port)
;opens a port to read the input file from
(define in (open-input-file "input2.txt"))

;splits a list into sublist by a given delimiter
;https://codereview.stackexchange.com/questions/87058/splitting-a-list-in-racket
(define (split-by lst x)
  (foldr (lambda (element next)
           (if (eq? element x)
               (cons '() next)
               (cons (cons element (car next)) (cdr next))))
         (list '()) lst))

;formats every list with a monkey in the following way:
(define (format l)
   (list ;(" 76, 88, 96, 97, 58, 61, 67") -> (76 88 96 97 58 61 67)
         (map string->number (apply append (map (lambda(y)(string-split char-space? y)) (string-split char-comma? (car (car l))))))
         ;(" new = old * 19") -> ("old" "*" "19")
         (string-split char-space? (car (cdr (string-split char-eq? (car (car (cdr l)))))))
         ;(" divisible by 3") -> (3)
         (map string->number (cdr (cdr (string-split char-space? (car (car (cdr (cdr l))))))))
         ;(" throw to monkey 2") -> (2)
         (map string->number (cdr(cdr (cdr (string-split char-space? (car (car(cdr (cdr (cdr l))))))))))
         ;(" throw to monkey 3") -> (3)
         (map string->number (cdr(cdr (cdr (string-split char-space? (car (car (cdr (cdr (cdr (cdr l)))))))))))))

;replaces every occurance of "old" in l with the element el
(define (subs el l)
  (if (null? l)
      '()
       (if (string=? "old" (car l))
           (cons el (subs el (cdr l)))
           (if (string->number (car l))
               (cons (string->number (car l)) (subs el (cdr l)))
            (cons (car l) (subs el (cdr l)))))))

;executes an operation for every element,that the monkey is holding
(define (do-operation l)
  (cond ((string=? "+" (car (cdr l))) (+ (car l) (car (cdr (cdr l)))))
        ((string=? "*" (car (cdr l))) (* (car l) (car (cdr (cdr l)))))
        ((string=? "/" (car (cdr l))) (/ (car l) (car (cdr (cdr l)))))
        ((string=? "-" (car (cdr l))) (- (car l) (car (cdr (cdr l)))))
        ))

;returns a list where the first element of m is replaced by a list containing the result for every element after the operations have been executed
(define (handle-monkey  m)
  (append (list (map (lambda(x) (do-operation (subs x (car (cdr m))))) (car m))) (cdr m))
  )

;checks if an element (after an operation has been executed) is divisible by num
;the following pair is returned, based on the result of the division : (element / 3, #t/#f position)
(define (item el m)
  (if (= 0 (remainder (quotient el 3) (car (car (cdr (cdr m))))))
      (cons (quotient el 3) (car (cdr (cdr (cdr m)))))
      (cons (quotient el 3) (car (cdr (cdr (cdr (cdr m))))))))

;every element that m holds gets replaced by the following pair : (item element l)
(define (mapIt m)
(append (list (map (lambda(x) (item x m)) (car (handle-monkey m)))) (cdr m) ))

;adds the given element to the list of elements of monkey number-'i' of l
(define (addit l ind el)
  (list (append (list (append (car (list-ref l ind)) (list el))) (cdr (list-ref l ind)))))

;clears the list of elements of every monkey, after thet have been visited and thrown away
;the number of elements thrown are being appended to the end of every monkey's list 
(define (empty-item-list i l)
  (append (sublist l 0 (- i 1)) (list (append (list '()) (append (cdr(list-ref l i))(list (length (car (list-ref l i))))))) (sublist l (+ 1 i) (- (length l) 1)) ))

;the items of every monkey get inspected and thrown 
(define (handle  monkey l)
  (define (helper m res)
    (if (null? m)
        res
        (helper (cdr m) (append (sublist res 0 (- (car (cdr (car m))) 1)) (addit res (car (cdr (car m))) (car (car m))) (sublist res (+ 1 (car (cdr (car m)))) (- (length res) 1))))
        ))(helper (car monkey) l))

;goes through 1 round and applies the 'handle' function to every monkey
(define (loopIt l)
  (define (helper  i res)
    (if ( = i (length l))
        res
        (helper (+ i 1) (empty-item-list i (handle (mapIt (list-ref res i)) res)) )
        )
    )
  (helper 0 l)
  )

;goes through all 20 rounds
(define (20-rounds l)
  (define (helper cnt res)
    (if (= cnt 21)
       res
        (helper (+ 1 cnt) (loopIt res))))
  (helper 1 l))

;insertion sort
(define (insertion-sort l)
  (define (insert x l)
    (cond ((null? l) (list x))
          ((>= x (car l))
           (cons x l))
          (else
            (cons (car l) (insert x (cdr l))))))
  (foldr insert '() l))

;the function goes through all 20 rounds for list l
;it takes the last element of every list (it shows how many items the monkey has held for all rounds)
;sorts the last elements in ascending order
;the first 2 elements get multiplied which gives the result
(define (calc l)
  (foldr * 1 (sublist (insertion-sort (map (lambda(z)(foldr + 0 z))  (map (lambda(x) (filter (lambda(y) (eq? #f (list? y))) x)) l))) 0 1)))


;read the input
(define input (port->string in))

;splits the input string intp a list of strings using newline character as a delimiter
(define inp (string-split char-newline? input))
;-> ("Monkey 0:\r"
; "  Starting items: 79, 98\r"
; "  Operation: new = old * 19\r"
; "  Test: divisible by 23\r"
; "    If true: throw to monkey 2\r"
; "    If false: throw to monkey 3\r"
; "\r"
; "Monkey 1:\r" ... )

;splits the list of strings into a list of lists, each containing a string, using the return character as a delimiter
(define inp2 (map (lambda(x) (string-split char-return? x)) inp))
;-> (("Monkey 0:")
; ("  Starting items: 79, 98")
; ("  Operation: new = old * 19")
; ("  Test: divisible by 23")
; ("    If true: throw to monkey 2")
; ("    If false: throw to monkey 3")
; ()
; ("Monkey 1:")...)

;groups every 6 lists into a monkey-list 
(define inp3 (split-by inp2 '()))
;-> ((("Monkey 0:")
;  ("  Starting items: 79, 98")
;  ("  Operation: new = old * 19")
;  ("  Test: divisible by 23")
;  ("    If true: throw to monkey 2")
;  ("    If false: throw to monkey 3"))
; (("Monkey 1:")...)..)

;;splits every string element using ':' ad a delimiter
(define inp4 (map  (lambda(x) (map  (lambda(y) (string-split char-dots? (car y)))  x))  inp3))
;-> ((("Monkey 0")
;  ("  Starting items" " 79, 98")
;  ("  Operation" " new = old * 19")
;  ("  Test" " divisible by 23")
;  ("    If true" " throw to monkey 2")
;  ("    If false" " throw to monkey 3"))
; (("Monkey 1")

;returns the second parts of the lists after they have been split by ':'
(define inp5 (map (lambda(x) (map cdr (cdr x))) inp4))
;-> (((" 79, 98") (" new = old * 19") (" divisible by 23") (" throw to monkey 2") (" throw to monkey 3"))
;   ((" 54, 65, 75, 74") (" new = old + 6") (" divisible by 19") (" throw to monkey 2") (" throw to monkey 0"))

;formats every monkey-list using the function 'format'
(define inp6 (map format inp5))
;-> (((79 98) ("old" "*" "19") (23) (2) (3))
;   ((54 65 75 74) ("old" "+" "6") (19) (2) (0))

;calculates the result
(display (calc (20-rounds inp6)))
(close-input-port in)