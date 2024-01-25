#lang r5rs
(#%require "../commonFunctions.rkt")
(#%require racket/port)
;opens a port to read the input file from
(define in (open-input-file "input2.txt"))


;makes pairs of elements
(define (make-pairs l)
  (if (null? l)
     '()
     (append (list (cons (car l) (if (null? (car (cdr l)))
                             '()
                             (car (cdr l))))) (make-pairs (cdr (cdr l))))))

;determines the result of a round based on the given rules
(define (score-round r)
  (cond ((string=? "X" (cdr r)) (+ 1 (if (string=? "A" (car r))
                                       3
                                       (if (string=? "B" (car r))
                                           0
                                           6))))
        ((string=? "Y" (cdr r)) (+ 2 (if (string=? "A" (car r))
                                       6
                                       (if (string=? "B" (car r))
                                           3
                                           0))))
        (else ( + 3 (if (string=? "A" (car r))
                                       0
                                       (if (string=? "B" (car r))
                                           6
                                           3))))))

;splits the input by new lines
(define s (string-split char-whitespace? (port->string in)))
;-> ("A" "Y" "B" "X" "B" "X" "C" "Y" "B" "X" "A" "Z" "B" "X" "B" "X" "C" "Z")

;makes pairs of every two elements in the list
(define pairs (make-pairs s))
;-> (("A" . "Y") ("B" . "X") ("B" . "X") ("C" . "Y") ("B" . "X") ("A" . "Z") ("B" . "X") ("B" . "X") ("C" . "Z"))

;determines the score of every pair
(define scores (map score-round (make-pairs s)))
;-> (8 1 1 2 1 3 1 1 6)

;calculates the total score points
(display (foldr + 0 scores))

(close-input-port in)