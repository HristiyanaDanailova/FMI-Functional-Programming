#lang r5rs
(#%require "../commonFunctions.rkt")
(#%require racket/port)
;opens a port to read the input file from
(define in (open-input-file "input2.txt"))

;checks if the desired amount of cycles is achieved
(define (cycCheck cyc sum)
  (if (or (= cyc 20)
          (= cyc 60)
          (= cyc 100)
          (= cyc 140)
          (= cyc 180)
          (= cyc 220))
      #t
      #f))

;executes every instrunction
;for every amound of cycles achieved it recordes a pair of type: (cycle (sum * cycle)) in res
(define (action l)
  (define (helper cycles sum res ls)
    (cond ((null? ls) res)
          ((string=? "noop" (car ls)) (helper (+ 1 cycles) sum (if (eq? (cycCheck cycles sum) #f)
                                                                   res
                                                                   (append res (list (cons cycles (* sum cycles))))) (cdr ls)))
          ((string=? "addx" (car ls)) (helper (+ cycles 2) (+ sum (string->number (car (cdr ls)))) (if (eq? (cycCheck cycles sum) #f)
                                                                                              (if (eq? (cycCheck (+ cycles 1) sum) #f)
                                                                                                  res
                                                                                                  (append  res (list (cons (+ 1 cycles) (* (+ 1 cycles) sum)))))
                                                                                                  (append  res (list (cons cycles (* cycles sum))))) (cdr (cdr ls))))
          
      ))
  (helper 1 1 '() l))

;reads the input
(define input (port->string in))
;-> "addx 15\r\naddx -11\r\naddx 6\r\naddx -3\r\naddx 5\r\naddx -1\r\naddx -8\r\naddx 13\r\naddx 4\r\nnoop\r\naddx -1"

;splits the input string into a list of strings using whitespace as a delimiter
(define splitInp (string-split char-whitespace? input))
;-> ("addx" "15" "addx" "-11" "addx" "6" "addx" "-3" "addx" "5" "addx" "-1" "addx" "-8" "addx" "13" "addx" "4" "noop" "addx" "-1")

;performs the instructions
(define cycles (action splitInp))
;-> ((20 . 420))

;finds the sum of the six signal-strengths
(display (foldr + 0 (map cdr cycles)))

(close-input-port in)
