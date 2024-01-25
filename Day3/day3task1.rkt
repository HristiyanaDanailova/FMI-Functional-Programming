#lang r5rs
(#%require "../commonFunctions.rkt")
(#%require racket/port)
;opens a port to read the input file from
(define in (open-input-file "input2.txt"))


;splits the list in half (l -> (list1 list2)) and filters all elements from list1 that appear in list2
(define (match l)
  (filter (lambda(x) (charInStr? x (sublist l (/ (length l) 2) (length l)))) (sublist l 0 (- (/ (length l) 2) 1))))
;(#\D #\m #\p #\t #\n #\g #\t #\F #\w #\v #\v #\M #\m #\w #\m #\m) -> ( (list1 #\D #\m #\p #\t #\n #\g #\t #\F) (list2 #\w #\v #\v #\M #\m #\w #\m #\m) ) -> (#\m)


;reads the input file
(define input (port->string in))
;-> "WVHGHwddqSsNjsjwqVvdwZRCbcJcZTCcsZbLcJJsCZ\r\nhngprFFhFDFhrDpzzQDhtnBJJRJZbZvTcvbfRCJfBRcBJl\r\nDmptngtFwvvMmwmm\r\nHFddrJnLdqtHBMQBmmVm"

;splits the input string into list of strings using the new line character as a delimiter
(define inputToStrs (string-split char-whitespace? input))
;-> ("WVHGHwddqSsNjsjwqVvdwZRCbcJcZTCcsZbLcJJsCZ" "hngprFFhFDFhrDpzzQDhtnBJJRJZbZvTcvbfRCJfBRcBJl" "DmptngtFwvvMmwmm" "HFddrJnLdqtHBMQBmmVm")

;splits every string into a list of chars
(define lists (map string->list inputToStrs))
;-> ((#\W #\V #\H #\G #\H #\w #\d #\d #\q #\S #\s #\N #\j #\s #\j #\w #\q #\V #\v #\d #\w #\Z #\R #\C #\b #\c #\J #\c #\Z #\T #\C #\c #\s #\Z #\b #\L #\c #\J #\J #\s #\C #\Z)
 ; (#\h #\n #\g #\p #\r #\F #\F #\h #\F #\D #\F #\h #\r #\D #\p #\z #\z #\Q #\D #\h #\t #\n #\B #\J #\J #\R #\J #\Z #\b #\Z #\v #\T #\c #\v #\b #\f #\R #\C #\J #\f #\B #\R #\c #\B #\J #\l)
 ;(#\D #\m #\p #\t #\n #\g #\t #\F #\w #\v #\v #\M #\m #\w #\m #\m)
 ;(#\H #\F #\d #\d #\r #\J #\n #\L #\d #\q #\t #\H #\B #\M #\Q #\B #\m #\m #\V #\m))

;finds the repeating character in every string using match and maps it's priority 
(define matches
  (map decode (map car (map (lambda(x) (match x)) lists))))
;-> (19 28 13 34)

;sums the priorities of all elements
(display (foldr + 0 matches))

(close-input-port in)