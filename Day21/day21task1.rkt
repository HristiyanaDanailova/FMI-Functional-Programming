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

;helper function for a tree structure
;works with a tree that has two children - left and right
(define the-empty-tree '())
(define (make-tree root left right)
(list root left right))
(define root-t car)
(define left-t cadr)
(define right-t caddr)

;deletes 'item' from 'list'
(define (delete item list) (filter (lambda (x) (not (equal? x item))) list))

;looks for an element in a left subtree
(define (findLeft l r)
  (if (string->number (car (cdr r))) ;if the values is a number, it is returned
      (list (car (cdr r)))
  (car (filter (lambda(x) (string=? (car x) (car (cdr r)))) l)))) ;if the value isn't a number, the monkey gets returned

;looks for an element in a right subtree
(define (findRight l r)
  (if (string->number (car (cdr (cdr r)))) ;if the values is a number, it is returned
      (list (car (cdr (cdr r))))
  (car (filter (lambda(x) (string=? (car x) (car (cdr (cdr (cdr r)))))) l)))) ;if the value isn't a number, the monkey gets returned

;creates a tree with a root-element 'r' and elements from the list l
(define (make-tree-list l r)
  (if (null? l)
      '()
      (if (= 2 (length r)) ;if the vlength is 2, then there is a monkey with a value
          (make-tree (car (cdr r)) the-empty-tree the-empty-tree)
      (if (= 1 (length r)) ;if the length is 1, then there is a value
          (make-tree (car r) the-empty-tree the-empty-tree)
          (make-tree (car (cdr (cdr r))) ;in this case there is a monkey with an operation; a left and right trees get recursively constructed with a root-the operation
                     (make-tree-list (delete (findLeft l r) l) (findLeft l r))
                     (make-tree-list (delete (findRight l r) l) (findRight l r)))

                  ))))

;applies the operation op to the l and r elements
(define (do-oper op l r)
  (cond ((string=? "+" op) (+ l r))
        ((string=? "*" op) (* l r))
        ((string=? "/" op) (quotient l r))
        ((string=? "-" op) (- l r))
    ))

;evaluates the tree
(define (evalTree t)
  (if (null? t)
      '()
      (if (string->number (root-t t))  ;if it's a number it gets returned
           (string->number (root-t t))
              (do-oper   (root-t t) ;the operation gets applied to the left and right children
                         (evalTree (left-t t))
                         (evalTree (right-t t)))))

  )

;reads the input
(define inp (port->string in))
;-> "root: pppw + sjmn\r\ndbpl: 5\r\ncczh: sllz + lgvd\r\nzczc: 2\r\nptdq: humn - dvpt\r\ndvpt: 3\r\nlfqf: 4\r\nhumn: 5\r\nljgn: 2\r\nsjmn: drzm * dbpl\r\nsllz: 4\r\npppw: cczh / lfqf\r\nlgvd: ljgn * ptdq\r\ndrzm: hmdt - zczc\r\nhmdt: 32"

;splits the input string into lists of characters, using the space character as a delimiter
(define inp2 (map (lambda(t) (split-by t #\space))(map car (map (lambda(y) (filter (lambda(z) (not (null? z))) y))(map (lambda(x) (split-by x #\newline))(split-by (string->list inp) #\return))
))))
;-> (((#\r #\o #\o #\t #\:) (#\p #\p #\p #\w) (#\+) (#\s #\j #\m #\n))
;   ((#\d #\b #\p #\l #\:) (#\5))
;   ((#\c #\c #\z #\h #\:) (#\s #\l #\l #\z) (#\+) (#\l #\g #\v #\d))
;   ((#\z #\c #\z #\c #\:) (#\2))
;   ((#\p #\t #\d #\q #\:) (#\h #\u #\m #\n) (#\-) (#\d #\v #\p #\t))
;   ((#\d #\v #\p #\t #\:) (#\3))
;   ((#\l #\f #\q #\f #\:) (#\4))
;   ((#\h #\u #\m #\n #\:) (#\5))
;   ((#\l #\j #\g #\n #\:) (#\2))
;   ((#\s #\j #\m #\n #\:) (#\d #\r #\z #\m) (#\*) (#\d #\b #\p #\l))
;   ((#\s #\l #\l #\z #\:) (#\4))
;   ((#\p #\p #\p #\w #\:) (#\c #\c #\z #\h) (#\/) (#\l #\f #\q #\f))
;   ((#\l #\g #\v #\d #\:) (#\l #\j #\g #\n) (#\*) (#\p #\t #\d #\q))
;   ((#\d #\r #\z #\m #\:) (#\h #\m #\d #\t) (#\-) (#\z #\c #\z #\c))
;   ((#\h #\m #\d #\t #\:) (#\3 #\2)))

;removes the ':' character from the lists
(define no-dots (map (lambda(x) (map (lambda(y) (filter (lambda(z) (not (char=? #\: z))) y)) x))inp2))
;-> (((#\r #\o #\o #\t) (#\p #\p #\p #\w) (#\+) (#\s #\j #\m #\n))
;   ((#\d #\b #\p #\l) (#\5))
;   ((#\c #\c #\z #\h) (#\s #\l #\l #\z) (#\+) (#\l #\g #\v #\d))
;   ((#\z #\c #\z #\c) (#\2))
;   ((#\p #\t #\d #\q) (#\h #\u #\m #\n) (#\-) (#\d #\v #\p #\t))
;   ((#\d #\v #\p #\t) (#\3))
;   ((#\l #\f #\q #\f) (#\4))
;   ((#\h #\u #\m #\n) (#\5))
;   ((#\l #\j #\g #\n) (#\2))
;   ((#\s #\j #\m #\n) (#\d #\r #\z #\m) (#\*) (#\d #\b #\p #\l))
;   ((#\s #\l #\l #\z) (#\4))
;   ((#\p #\p #\p #\w) (#\c #\c #\z #\h) (#\/) (#\l #\f #\q #\f))
;   ((#\l #\g #\v #\d) (#\l #\j #\g #\n) (#\*) (#\p #\t #\d #\q))
;   ((#\d #\r #\z #\m) (#\h #\m #\d #\t) (#\-) (#\z #\c #\z #\c))
;   ((#\h #\m #\d #\t) (#\3 #\2)))

;transforms all lists of characters to strings
(define inp3 (map (lambda(x) (map list->string x)) no-dots))
;-> (("root" "pppw" "+" "sjmn")
;   ("dbpl" "5")
;   ("cczh" "sllz" "+" "lgvd")
;   ("zczc" "2")
;   ("ptdq" "humn" "-" "dvpt")
;   ("dvpt" "3")
;   ("lfqf" "4")
;   ("humn" "5")
;   ("ljgn" "2")
;   ("sjmn" "drzm" "*" "dbpl")
;   ("sllz" "4")
;   ("pppw" "cczh" "/" "lfqf")
;   ("lgvd" "ljgn" "*" "ptdq")
;   ("drzm" "hmdt" "-" "zczc")
;   ("hmdt" "32"))

;finds the number that the root-monkey yells
(display (evalTree (make-tree-list (delete (car (filter (lambda(x) (string=? "root" (car x))) inp3)) inp3) (car (filter (lambda(x) (string=? "root" (car x))) inp3)))))
(close-input-port in)