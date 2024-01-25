#lang r5rs
(#%require "../commonFunctions.rkt")
(#%require racket/port)
;opens a port to read the input file from
(define in (open-input-file "input2.txt"))

;helper functions returning a row,a column or an element
(define (get-row i m) (list-ref m i))
(define (get-col i m) (map (lambda(x)(list-ref x i)) m))
(define (get-el i j m)(get-row i (get-col j m)))

;chekcs if el is an element of the list l
;if it's found, its index is returned, else the number -1 is returned
(define (get-list-index l el)
    (if (null? l)
        -1
        (if (equal? (car l) el)
            0
            (let ((result (get-list-index (cdr l) el)))
                (if (= result -1)
                    -1
                    (+ 1 result))))))

;finds the element ch in the matrix m and returns a pair of type: (row column) where row and column are the indexes of the element
(define (findPos ch m)
  (define (helper res mr row)
    (if (null? mr)
        res
        (helper (if (not (null? (filter (lambda(x) (char=? x ch)) (car mr))))
                    (cons row (get-list-index (car mr) ch))
                    res
                    ) (cdr mr) (+ row 1))))
  (helper '() m 0))

;checks if the indexes i and j are valid and if they are, a pair of type: (row column) is returned
;if the indexes are unvalid the empty list is returned
(define (inside i j m)
  (if (and (< -1 i)
           (< i (length m))
           (< -1 j)
           (< j (length (car m))))
          (cons i j)
          '()))

;returns the neighbouring element of the element with positions (i j)
;the valid neighbouting elements have valid indexes (they are inside the matrix), haven't been visited yet and the height-difference is within the allowed range
(define (neighbours i j m visited) 
  (filter (lambda(z) (> (- (get-el i j m) (get-el (car z) (cdr z) m)) -2)) (filter (lambda(y) (= -1 (get-list-index visited y))) (filter (lambda(x) (not (null? x)))(list (inside (- i 1) j m)
        (inside (+ i 1) j m)
        (inside i (- j 1) m)
        (inside i (+ j 1) m)))
  )))

;for a list of type ('(list pointToVisit) '(list visited)), for every element of the list '(pointsToVisit) the functions adds the result of the function 'neighbours' to the result
;if '(pointsToVisit) is empty, a list of type: ('(res-> all neighbours of visited point) '(visited-> all visited points and the new visited points))
(define (neighPoints m points)
  (define (helper res visited ls)
    (if (null? ls)
     (list (unique res) (unique (append visited res)))
           (helper (append res (unique (neighbours (car (car ls)) (cdr(car ls)) m visited)))
              visited
              (cdr ls)
              )))(helper '() (car (cdr points)) (car points)))

;checks if an element is unique for a list
(define (unique? l x)
  (if (> (length (filter (lambda(y) (equal? x y)) l)) 1)
      #f
      #t))

;leaves only one occurance of every repeated element
(define (unique lst)
  (define (helper lst result)
    (cond
      ((null? lst) result)
      ((member (car lst) result) (helper (cdr lst) result))
      (else (helper (cdr lst) (append result (list (car lst)))))))
  (helper lst '()))

;the function takes the coordinates of a start-point and a matrix as arguments
;all the neighbour points of the start-point are found recursively (at 0+1 distance)
;then the neighbour points of the points of distance 0+1 are found (the new neighbouring points are at distance 0+2)
;this recursive process continues until the indexes of the final point appear in the visited list
;the number of steps gets returned
(define (pathIt i j m ie je)
 (define (helper steps nextPoints)
   (if (not (= -1 (get-list-index (car (cdr nextPoints)) (cons ie je))))
       steps
       (helper (+ steps 1) (neighPoints m nextPoints))))
 (helper  0 (list(list (cons i j) ) (list (cons i j) ))))

;reads the input
(define input (port->string in))
;-> "Sabqponm\r\nabcryxxl\r\naccszExk\r\nacctuvwj\r\nabdefghi"

;splits the input string into a list of strings using the newline character as a delimiter
(define inputToStrs (string-split char-whitespace? input))
;-> ("Sabqponm" "abcryxxl" "accszExk" "acctuvwj" "abdefghi")

;splits every string into a list of characters
(define matrix (map string->list inputToStrs))
;-> ((#\S #\a #\b #\q #\p #\o #\n #\m) (#\a #\b #\c #\r #\y #\x #\x #\l) (#\a #\c #\c #\s #\z #\E #\x #\k) (#\a #\c #\c #\t #\u #\v #\w #\j) (#\a #\b #\d #\e #\f #\g #\h #\i))

;maps the alphabet position to every character
(define mtr (map (lambda(y)(map (lambda(x) (if (char=? x #\S)
                                1
                                (if (char=? x #\E)
                                    26
                                    (decode x)))) y)) matrix))
;-> ((1 1 2 17 16 15 14 13) (1 2 3 18 25 24 24 12) (1 3 3 19 26 26 24 11) (1 3 3 20 21 22 23 10) (1 2 4 5 6 7 8 9))

;defines the indexes of the start-point
(define startPos (findPos #\S matrix))
;-> (0 . 0)

;defines the indexes of the end-point
(define endPos (findPos #\E matrix))
;-> (2 . 5)

;finds the fewest steps required to move from the start-point to the location that should get the best signal; works a bit slow
(display (pathIt (car startPos)(cdr startPos) mtr (car endPos) (cdr endPos)))

(close-input-port in)