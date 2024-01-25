#lang r5rs
(#%require "../commonFunctions.rkt")
(#%require racket/port)
;opens a port to read the input file from
(define in (open-input-file "input2.txt"))

;splits the list of sublist by a given delimiter
;https://codereview.stackexchange.com/questions/87058/splitting-a-list-in-racket
(define (split-by lst x)
  (foldr (lambda (element next)
           (if (eq? element x)
               (cons '() next)
               (cons (cons element (car next)) (cdr next))))
         (list '()) lst))

 ;by a given list of chars returns a list of string 
(define (make-lst l)
  (define (helper res ls)
    (if (null? ls)
        res
        (helper (append res (list (string (car ls)))) (cdr ls) )))
  (helper '() l))

;formats the input by skipping all symbols, different than numbers and chars
(define (format l)
  (define (helper curr res)
    (if (> curr (length l))
        res
        (helper (+ curr 4) (append res (list (list-ref l curr))))))
  (helper 1 '()))

;finds if there are elements being true for the given predicate p?
(define (any? p? lst)
  (foldr (lambda (x acc) (or (p? x) acc)) #f lst))

;n-ary zipWith
(define (zipWith f . lsts)
  (if (or (null? lsts)
          (any? null? lsts))
      '()
      (cons (apply f (map car lsts))
            (apply zipWith f (map cdr lsts)))))

;combines n-number of lists, grouping their values by index -> (list1 1 2) (list2 3 4) (list3 5 6) -> ((1 3 5) (2 4 6))
(define (combine l)
  (if (null? (filter (lambda(x) (not (null? x))) l))
     '()
     (append (list (zipWith car l)) (combine (map cdr l)))))

;splits a list of strings by a given string delimiter
(define (split-by-str lst x)
  (foldr (lambda (element next)
           (if (string=? element x)
               (cons '() next)
               (cons (cons element (car next)) (cdr next))))
         (list '()) lst))

;returns only the odd-positioned elements
(define (odd-ind l)
  (define (helper curr res)
    (if (= curr (length l))
        res
        (helper (+ curr 1) (if (odd? curr)
                               (append res (map (lambda(x) (apply string-append x)) (list (list-ref l curr))))
                               res))))
  (helper 0 '()))

;returns 'm'-number of elements from list 'f' in 't' 
(define (elem l m f t)
 (sublist (list-ref l (- f 1))  (- (length (list-ref l (- f 1)))  m) (- (length (list-ref l (- f 1))) 1)))

;ads the elements returned from function 'elem' to list 't'
(define (handle l m f t)
  (append (sublist l 0 (- t 2)) (list (append (list-ref l (- t 1)) (elem l m f t))) (sublist l t (- (length l)1 ))))

;removes the elements from the list 'f', which have been movet to the list 't'
(define (rm-els l m f)
  (append (sublist l 0 (- f 2)) (list (sublist (list-ref l (- f 1)) 0 ( - (length (list-ref l (- f 1)))(+ 1 m)))) (sublist l f (- (length l) 1))))

;performs a signle action on the given configuration of elements
(define (move cs l)
 (rm-els (handle l (car cs) (car (cdr cs)) (car (cdr (cdr cs))))  (car cs) (car (cdr cs))))

;performs all the required action on list l
(define (loop c l)
  (define (helper res cs)
    (if (null? cs)
        res
        (helper (move (car cs) res) (cdr cs))))
  (helper l c))

;reads the input file
(define inp (port->string in))
;-> "    [D]    \r\n[N] [C]    \r\n[Z] [M] [P]\r\n 1   2   3 \r\n\r\nmove 1 from 2 to 1\r\nmove 3 from 1 to 3\r\nmove 2 from 2 to 1\r\nmove 1 from 1 to 2"

;splits the input string into a list of characters
(define input (string->list inp))
;-> (#\space
;   #\space
;   #\space
;   #\space
;   #\[
;   #\D
;   #\]
;   #\space...)

;splits the input list of chars into a list of sublists, using the new line character as a delimiter
(define splitted (map (lambda(y) (make-lst y))(map (lambda(x) (filter (lambda(y) (not (char=? #\return y))) x)) (split-by input #\newline))))
;->  ("[" "N" "]" " " "[" "C" "]" " " " " " " " ")
;    ("[" "Z" "]" " " "[" "M" "]" " " "[" "P" "]")
;    (" " "1" " " " " " " "2" " " " " " " "3" " ")
;    ()
;    ("m" "o" "v" "e" " " "1" " " "f" "r" "o" "m" " " "2" " " "t" "o" " " "1")
;    ("m" "o" "v" "e" " " "3" " " "f" "r" "o" "m" " " "1" " " "t" "o" " " "3")
;    ("m" "o" "v" "e" " " "2" " " "f" "r" "o" "m" " " "2" " " "t" "o" " " "1")
;    ("m" "o" "v" "e" " " "1" " " "f" "r" "o" "m" " " "1" " " "t" "o" " " "2"))

;splits the list of lists into a list of two sublists - one, containing the elements configuratios, and the other - containing the commands (using the empty-list as a delimiter)
(define elems (split-by splitted '()))
;-> (((" " " " " " " " "[" "D" "]" " " " " " " " ")
;   ("[" "N" "]" " " "[" "C" "]" " " " " " " " ")
;   ("[" "Z" "]" " " "[" "M" "]" " " "[" "P" "]")
;   (" " "1" " " " " " " "2" " " " " " " "3" " "))
;   (("m" "o" "v" "e" " " "1" " " "f" "r" "o" "m" " " "2" " " "t" "o" " " "1")
;   ("m" "o" "v" "e" " " "3" " " "f" "r" "o" "m" " " "1" " " "t" "o" " " "3")
;   ("m" "o" "v" "e" " " "2" " " "f" "r" "o" "m" " " "2" " " "t" "o" " " "1")
;   ("m" "o" "v" "e" " " "1" " " "f" "r" "o" "m" " " "1" " " "t" "o" " " "2")))

;splits and groups the elements configuration by columns
(define combined (map (lambda(x) (reverse (filter (lambda(y) (not (string=? " " y))) x))) (combine (map format(car  elems)))))
;-> (("1" "Z" "N") ("2" "M" "C" "D") ("3" "P"))

;formats the commands-list by leaving in only the number values 
(define commands (map (lambda(y)( map string->number y)) (map odd-ind (map (lambda(x)(split-by-str x " "))(car (cdr elems))))))
;-> ((1 2 1) (3 1 3) (2 2 1) (1 1 2))

;returns the elements on the top from the result list after all commands have been  executed
(display (apply string-append (map (lambda(x) (car (reverse x)))(loop commands combined))))
;(the solution differs from the solution of day1 in the function elem - the result isn't returned in reverse order)

(close-input-port in)