#lang r5rs
(#%require "../commonFunctions.rkt")
(#%require racket/port)
;opens a port to read the input file from
(define in (open-input-file "input2.txt"))

;checks if a string is a number
(define (str-dig? str)
  (or (string=? "0" str)
      (string=? "1" str)
      (string=? "2" str)
      (string=? "3" str)
      (string=? "4" str)
      (string=? "5" str)
      (string=? "6" str)
      (string=? "7" str)
      (string=? "8" str)
      (string=? "9" str)
      (string=? "10" str)
      ))

;compares every pair of elements according to the given rules
(define (funcStr? l1 l2)
  (cond ((and (str-dig? (car l1))
              (str-dig? (car l2))) (if (< (string->number (car l1)) (string->number (car l2)))
                                         #t
                                         (if (> (string->number (car l1)) (string->number (car l2)))
                                             #f
                                             (funcStr? (cdr l1) (cdr l2)))))
        ((and (string=? "[" (car l1))
              (string=? "[" (car l2))) (funcStr? (cdr l1) (cdr l2)))

        ((and (string=? "[" (car l1))
              (string=? "]" (car l2))) #f)

        ((and (string=? "]" (car l1))
              (string=? "[" (car l2))) #t)

        ((and (string=? "]" (car l1))
              (str-dig? (car l2))) #t)
        
        ((and (string=? "]" (car l1))
              (string=? "]" (car l2)))  (funcStr? (cdr l1) (cdr l2)))

        ((and (str-dig? (car l1))
              (string=? "]" (car l2))) #f)
        
        ((and (str-dig? (car l1))
              (string=? "[" (car l2)))  (funcStr? (append (list "[" (car l1) "]") (cdr l1)) l2))
        
        ((and (string=? "[" (car l1))
              (str-dig? (car l2)))   (funcStr? l1 (append (list "[" (car l2)  "]") (cdr l2))))
        ))


;formats a list by transforming its elements to strings
(define (format l)
  (if (null? l)
      '()
      (cons (if (char=? (car l) #\1)
                (if (char=? (car (cdr l)) #\0)
                  "10"
                  (string (car l)))
                (if (not (char=? #\, (car l)))
                    (string (car l))
                    ""
                    )) (format (cdr l)))))

;skips all empty string elements in a list
(define (skip-empty l)
  (if (null? l)
      '()
      (if (string=? "" (car l))
          (skip-empty (cdr l))
          (cons (car l) (skip-empty (cdr l))))))

;insertion sort where the compare operationn is the funcStr? function
(define (insertion-sort l)
  (define (insert x l)
    (cond ((null? l) (list x))
          ((eq? #t (funcStr? x (car l)))
           (cons x l))
          (else
            (cons (car l) (insert x (cdr l))))))
  (foldr insert '() l))

;finds the product of the positions of the two searched elements
(define (match l)
  (define (helper curr res ls)
   (if (null? ls)
       res
       (helper (+ 1 curr) (if (or (equal? (car ls) (list "[" "[" "2" "]" "]"))
                                  (equal? (car ls) (list "[" "[" "6" "]" "]")))
                              (* res (+ 1 curr))
                              res) (cdr ls))))
  (helper 0 1 l))

;reads the input
(define input (port->string in))
;-> "[[[[8,7,8,5,4],6,[4,6]]],[6,[[]]],[]]\r\n[[[[8,0,0,7,1],[1],8]]]"

;splits the input string into a list of strings using the whitespace character as a delimiter
(define splitIn (string-split char-whitespace? input))
;-> ("[[[[8,7,8,5,4],6,[4,6]]],[6,[[]]],[]]" "[[[[8,0,0,7,1],[1],8]]]")

;includes the additional packets
(define splitInp (append splitIn (list "[[2]]" "[[6]]")))
;-> ("[[[[8,7,8,5,4],6,[4,6]]],[6,[[]]],[]]" "[[[[8,0,0,7,1],[1],8]]]" "[[2]]" "[[6]]")

;splits a list of strings into a list of lists containing characters
(define formated (map (lambda(x) (skip-empty (format (string->list x)))) splitInp))
;-> (("[" "[" "[" "[" "8" "7" "8" "5" "4" "]" "6" "[" "4" "6" "]" "]" "]" "[" "6" "[" "[" "]" "]" "]" "[" "]" "]")
;   ("[" "[" "[" "[" "8" "0" "0" "7" "1" "]" "[" "1" "]" "8" "]" "]" "]")
;   ("[" "[" "2" "]" "]")
;  ("[" "[" "6" "]" "]"))

;finds the decoder key
(display (match (insertion-sort formated)))


(close-input-port in)