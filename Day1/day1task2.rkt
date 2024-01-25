#lang r5rs
(#%require "../commonFunctions.rkt")
(#%require racket/port)
;opens a port to read the input file from
(define in (open-input-file "input2.txt"))

(define (insertion-sort l)
  (define (insert x l)
    (cond ((null? l) (list x))
          ((>= x (car l))
           (cons x l))
          (else
            (cons (car l) (insert x (cdr l))))))
  (foldr insert '() l))

;reads input data and converts it to a list of strings
(define inToStr (port->string in))
;-> "10130\r\n9819\r\n4257\r\n8400\r\n10994\r\n3758\r\n8326\r\n\r\n9002\r\n15602\r\n1193\r\n6805\r\n10797"
(define inStrSplit (string-split char-return? inToStr))
;-> ("10130" "\n9819" "\n4257" "\n8400" "\n10994" "\n3758" "\n8326" "\n" "\n9002" "\n15602" "\n1193" "\n6805" "\n10797")

;removes the \n char from every string
(define inToLst (cons (car inStrSplit)(map (lambda (x) (substring x 1 (string-length x))) (cdr inStrSplit))))
;-> ("10130" "9819" "4257" "8400" "10994" "3758" "8326" "" "9002" "15602" "1193" "6805" "10797")

;splits the list into sublists using the empty string as a seperator
(define inLstSplit (split-by inToLst ""))
;-> (("10130" "9819" "4257" "8400" "10994" "3758" "8326") ("9002" "15602" "1193" "6805" "10797"))

;converts every string to a number
(define toNum (map (lambda(x) (map string->number x)) inLstSplit))
;-> ((10130 9819 4257 8400 10994 3758 8326) (9002 15602 1193 6805 10797))

;sums every sublist
(define elSum (map (lambda(x) (foldr + 0 x)) toNum))
;-> (55684 43399)

;sorts the result list and returns the sum of the greatest three elements
(define sorted (insertion-sort elSum))
(display (+ (car sorted) (car (cdr sorted)) (car (cdr (cdr sorted)))))

(close-input-port in)