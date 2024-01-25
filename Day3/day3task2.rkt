#lang r5rs
(#%require "../commonFunctions.rkt")
(#%require racket/port)
;opens a port to read the input file from
(define in (open-input-file "input2.txt"))


;helper functtions returning the first, second and third element of a list
(define (fst l) (car l))
(define (scd l) (car (cdr l)))
(define (trd l) (car (cdr (cdr l))))

;groups every three lists of a list (l -> (l1 l2 l3 l4 l5 l6)) -> ( (l1 l2 l3) (l4 l5 l6) )
(define (group l)
  (define (helper curr res ls)
    (if (= curr (length l))
        res
        (helper (+ curr 3) (append res (list (list (fst ls) (scd ls) (trd ls)))) (cdr (cdr (cdr ls)))
        )))
  (helper 0 '() l))

;finds the character that appears in all three lists
(define (match l)
  (filter (lambda(x) (and (charInStr? x (scd l)) (charInStr? x (trd l)))) (fst l)))


;reads the input file
(define input (port->string in))
;-> "vJrwpWtwJgWrhcsFMMfFFhFp\r\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\r\nPmmdzqPrVvPwwTWBwg\r\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\r\nttgJtRGJQctTZtZT\r\nCrZsJsPPZsGzwwsLwLmpwMDw"

;splits the input string into list of strings using the new line character as a delimiter
(define inputToStrs (string-split char-whitespace? input))
;-> ("vJrwpWtwJgWrhcsFMMfFFhFp" "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL" "PmmdzqPrVvPwwTWBwg" "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn" "ttgJtRGJQctTZtZT" "CrZsJsPPZsGzwwsLwLmpwMDw")

;splits every string into a list of chars
(define inputToLsts (map string->list inputToStrs))
;-> ((#\v #\J #\r #\w #\p #\W #\t #\w #\J #\g #\W #\r #\h #\c #\s #\F #\M #\M #\f #\F #\F #\h #\F #\p)
;   (#\j #\q #\H #\R #\N #\q #\R #\j #\q #\z #\j #\G #\D #\L #\G #\L #\r #\s #\F #\M #\f #\F #\Z #\S #\r #\L #\r #\F #\Z #\s #\S #\L)
;   (#\P #\m #\m #\d #\z #\q #\P #\r #\V #\v #\P #\w #\w #\T #\W #\B #\w #\g)
;   (#\w #\M #\q #\v #\L #\M #\Z #\H #\h #\H #\M #\v #\w #\L #\H #\j #\b #\v #\c #\j #\n #\n #\S #\B #\n #\v #\T #\Q #\F #\n)
;   (#\t #\t #\g #\J #\t #\R #\G #\J #\Q #\c #\t #\T #\Z #\t #\Z #\T)
;   (#\C #\r #\Z #\s #\J #\s #\P #\P #\Z #\s #\G #\z #\w #\w #\s #\L #\w #\L #\m #\p #\w #\M #\D #\w))

;groups every three lists into one sublist
(define groups (group inputToLsts))
;-> (((#\v #\J #\r #\w #\p #\W #\t #\w #\J #\g #\W #\r #\h #\c #\s #\F #\M #\M #\f #\F #\F #\h #\F #\p)
;   (#\j #\q #\H #\R #\N #\q #\R #\j #\q #\z #\j #\G #\D #\L #\G #\L #\r #\s #\F #\M #\f #\F #\Z #\S #\r #\L #\r #\F #\Z #\s #\S #\L)
;   (#\P #\m #\m #\d #\z #\q #\P #\r #\V #\v #\P #\w #\w #\T #\W #\B #\w #\g))
;   ((#\w #\M #\q #\v #\L #\M #\Z #\H #\h #\H #\M #\v #\w #\L #\H #\j #\b #\v #\c #\j #\n #\n #\S #\B #\n #\v #\T #\Q #\F #\n)
;   (#\t #\t #\g #\J #\t #\R #\G #\J #\Q #\c #\t #\T #\Z #\t #\Z #\T)
;   (#\C #\r #\Z #\s #\J #\s #\P #\P #\Z #\s #\G #\z #\w #\w #\s #\L #\w #\L #\m #\p #\w #\M #\D #\w)))

;finds the repeating characters for every group of three lists
(define resList (map car (map match groups)))
;-> (#\r #\Z)

;maps the priority to every character
(define resDecode (map decode resList))
;-> (18 52)

;sums the priorities of all elements
(display (foldr + 0 resDecode))


(close-input-port in)
