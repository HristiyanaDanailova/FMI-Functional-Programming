#lang r5rs
(#%require "../commonFunctions.rkt")
(#%require racket/port)
;opens a port to read the input file from
(define in (open-input-file "input2.txt"))

;ckecks if the position of curr is in the range of sprite
(define (act curr spr)
  (if (and (>= curr (- spr 1))
           (<= curr (+ spr 1)))
    (list #\#)
    (list #\.)))

;takes the list l and the last position of sprite-s
;iterates the 40 positions and for every one check if it is in the range of sprite, using the function 'act'
;the else case changes the position of sprite
(define (action l s)
  (define (helper curr spr res ls cyc)
    (cond ((= curr 40) (display res)
                       (newline)
                       spr)
          ((string=? "addx" (car ls)) (helper (+ curr 1) spr (append res (act curr spr)) (cdr ls) (+ cyc 1)))
          ((string=? "noop" (car ls)) (helper (+ curr 1) spr (append res (act curr spr)) (cdr ls) (+ cyc 1)))
          (else (helper (+ curr 1) (+ spr (string->number (car ls))) (append res (act curr spr)) (cdr ls) (+ cyc 1)))
        )
    )
  (helper 0 s '() l 1)
  )

;divides the list l into sublists of length 40 and passes every one of them to the function action
;helper gets s-the last position of sprite, as an argument; s is the result of the function 'action'
(define (loop-it l)
  (define (helper curr s)
    (if (= curr (length l))
        (display "Pixels have been rendered!") 
        (helper (+ curr 40) (action (sublist l curr (+ curr 39)) s))
         ))
  (helper 0 1))



;reads the input
(define input (port->string in))
;-> "addx 15\r\naddx -11\r\naddx 6\r\naddx -3\r\naddx 5\r\naddx -1\r\naddx -8\r\naddx 13\r\naddx 4\r\nnoop\r\naddx -1"

;splits the input string into a list of strings using whitespace as a delimiter
(define splitInp (string-split char-whitespace? input))
;-> ("addx" "15" "addx" "-11" "addx" "6" "addx" "-3" "addx" "5" "addx" "-1" "addx" "-8" "addx" "13" "addx" "4" "noop" "addx" "-1")

;returns the result line by line
(loop-it splitInp)

(close-input-port in)
