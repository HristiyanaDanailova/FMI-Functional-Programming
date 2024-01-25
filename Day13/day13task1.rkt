#lang r5rs
(#%require "../commonFunctions.rkt")
(#%require racket/port)
;opens a port to read the input file from
(define in (open-input-file "input2.txt"))

;groups the elements of a list 2 by 2
(define (group l)
  (define (helper curr res ls)
    (if (= curr (length l))
        res
        (helper (+ curr 2) (append res (list (list (car ls) (car (cdr ls))))) (cdr (cdr ls)))))
  (helper 0 '() l))

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

;sums the indexes of all elements that are in the right order
(define (iter l)
  (define (helper curr sum)
    (if (= curr (length l))
        sum
        (helper (+ curr 1) (if (eq? #t (list-ref l curr))
                               (+ sum (+ 1 curr))
                               sum))))
  (helper 0 0))

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

;reads the input
(define input (port->string in))
;-> "[[[[8,7,8,5,4],6,[4,6]]],[6,[[]]],[]]\r\n[[[[8,0,0,7,1],[1],8]]]"

;splits the input string into a list of strings using the whitespace character as a delimiter
(define splitInp (string-split char-whitespace? input))
;-> ("[[[[8,7,8,5,4],6,[4,6]]],[6,[[]]],[]]" "[[[[8,0,0,7,1],[1],8]]]")

;splits a list of strings into a list of lists containing characters
(define prs (map (lambda(x) (list (skip-empty (format (string->list (car x)))) (skip-empty(format (string->list (car (cdr x))))))) (group splitInp)))
;-> ((("[" "[" "[" "[" "8" "7" "8" "5" "4" "]" "6" "[" "4" "6" "]" "]" "]" "[" "6" "[" "[" "]" "]" "]" "[" "]" "]")
;   ("[" "[" "[" "[" "8" "0" "0" "7" "1" "]" "[" "1" "]" "8" "]" "]" "]"))

;sums the indexes of the pairs that are in the right order
(display (iter (map  (lambda(x) (funcStr? (car x) (car (cdr x)))) prs)))


(close-input-port in)