#lang r5rs
(#%provide (all-defined))

;compares a char with the #\return char
(define (char-return? ch)
  (char=? #\return ch))

;compares a char with the #\newline char
(define (char-newline? ch)
  (char=? #\newline ch))

;splits a string into a list of strings by a given delimiter
;https://cookbook.scheme.org/split-string/
(define (string-split char-delimiter? string)
  (define (maybe-add a b parts)
    (if (= a b) parts (cons (substring string a b) parts)))
  (let ((n (string-length string)))
    (let loop ((a 0) (b 0) (parts '()))
      (if (< b n)
          (if (not (char-delimiter? (string-ref string b)))
              (loop a (+ b 1) parts)
              (loop (+ b 1) (+ b 1) (maybe-add a b parts)))
          (reverse (maybe-add a b parts))))))

;splits a list into sublists by a given delimiter
;https://codereview.stackexchange.com/questions/87058/splitting-a-list-in-racket
(define (split-by lst x)
  (foldr (lambda (element next)
           (if (string=? element x)
               (cons '() next)
               (cons (cons element (car next)) (cdr next))))
         (list '()) lst))

;checks if the character 'ch' is a part of the string 'str'
(define (charInStr? ch str)
  (if (null? str)
       #f
       (if (char=? ch (car str))
            #t
            (charInStr? ch (cdr str)))))

;maps the ascii code to the character 'ch'
(define (decode ch)
  (if (char-lower-case? ch)
      (- (char->integer ch) 96)
      (- (char->integer ch) 38)))

;compares a char with the #\, char
(define (char-comma? ch)
  (char=? #\, ch))

;compares a char with the #\- char
(define (char-dash? ch)
  (char=? #\- ch))


;compares a char with the #\: char
(define (char-dots? ch) (char=? #\: ch))

;compares a char with the #\space char
(define (char-space? ch) (char=? #\space ch))

;compares a char with the #\= char
(define (char-eq? ch) (char=? #\= ch))

;returns a sublist of the elements from index s to index e
(define (sublist l s e)
  (define (helper ls curr res)
    (if (null? ls)
        res
        (helper (cdr ls) (+ curr 1) (if (and (>= curr s) (<= curr e))
                                        (append res (list (car ls)))
                                        res
                ))))
  (if (or ( < s 0) (< e s) (> s (length l)))
      '()
  (helper l 0 '())))

;fold function
(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

;filter function
(define (filter p l)
  (cond ((null? l) l)
        ((p (car l)) (cons (car l) (filter p (cdr l))))
        (else (filter p (cdr l)))))

