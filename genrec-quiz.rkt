;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname genrec-quiz) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require racket/list) ;gets list-ref, take and drop

 PROBLEM 1:
 
 ; In the lecture videos we designed a function to make a Sierpinski triangle fractal
 ; Design a function to create this circle fractal of size n and colour c.

(define CUTOFF 5)

;; Natural String -> Image
;; produce a circle fractal of size n and colour c
(check-expect (circle-fractal CUTOFF "black") (circle CUTOFF "outline" "black"))
(check-expect (circle-fractal (* 2 CUTOFF) "black")
              (local [(define sml (circle CUTOFF "outline" "black"))
                      (define big (circle (* 2 CUTOFF) "outline" "black"))]

                (overlay (beside sml sml) big)))
 
(define (circle-fractal n c)
  (cond [(<= n CUTOFF) (circle n "outline" c)]
        [else
         (local [(define small (circle-fractal (/ n 2) c))
                 (define big (circle n "outline" c))]

           (overlay big (beside small small)))])) 
 

 
  
; PROBLEM 2:
; 
; Below you will find some data definitions for a tic-tac-toe solver. 
; 
; In this problem we want you to design a function that produces all 
; possible filled boards that are reachable from the current board. 
; 
; In actual tic-tac-toe, O and X alternate playing. For this problem
; you can disregard that. You can also assume that the players keep 
; placing Xs and Os after someone has won. This means that boards that 
; are completely filled with X, for example, are valid.
; 
; Note: As we are looking for all possible boards, rather than a winning 
; board, your function will look slightly different than the solve function 
; you saw for Sudoku in the videos, or the one for tic-tac-toe in the 
; lecture questions.
;
;  PROBLEM 3:
; 
; Now adapt your solution to filter out the boards that are impossible if 
; X and O are alternating turns. You can continue to assume that they keep 
; filling the board after someone has won though. 
; 
; You can assume X plays first, so all valid boards will have 5 Xs and 4 Os.



;; Value is one of:
;; - false
;; - "X"
;; - "O"
;; interp. a square is either empty (represented by false) or has and "X" or an "O"

(define (fn-for-value v)
  (cond [(false? v) (...)]
        [(string=? v "X") (...)]
        [(string=? v "O") (...)]))

;; Board is (listof Value)
;; a board is a list of 9 Values
(define B0 (list false false false
                 false false false
                 false false false))

(define B1 (list false "X"   "O"   ; a partly finished board
                 "O"   "X"   "O"
                 false false "X")) 

(define B2 (list "X"  "X"  "O"     ; a board where X will win
                 "O"  "X"  "O"
                 "X" false "X"))

(define B3 (list "X" "O" "X"       
                 "O" "O" false
                 "X" "X" false))

(define B4 (list "X" "O" "X"       
                 "O" "O" "X"
                 "X" "X" "0"))

(define LOB2 (list
              (list "X"  "X"  "O"     
                    "O"  "X"  "O"
                    "X"  "X" "X")

              (list "X"  "X"  "O"     
                    "O"  "X"  "O"
                    "X"  "0" "X")))

(define (fn-for-board b)
  (cond [(empty? b) (...)]
        [else 
         (... (fn-for-value (first b))
              (fn-for-board (rest b)))]))



;; List Index Value -> (listof Values)
;; Produces a new list with a given value inserted at the specified index
;; NOTE: Value can be anything
(check-expect (fill-square (list 0 1 2 false 4) 3 3) (list 0 1 2 3 4))

(define (fill-square bd p nv)
  (append (take bd p)
          (list nv)
          (drop bd (add1 p))))




;; Board -> (listof Board)
;; Produce a list of all possible filled boards that are reachable from the given board
;; NOTE: Ignore the win condition
(check-expect (solve B2) (list
                          (list "X"  "X"  "O"    
                                "O"  "X"  "O"
                                "X"  "O"  "X")))

(check-expect (solve B3) (list
                          (list "X" "O" "X"       
                                "O" "O" "X"
                                "X" "X" "O")
                          
                          (list "X" "O" "X"       
                                "O" "O" "O"
                                "X" "X" "X")))

(define (solve bd)
  (local [(define (solve--bd bd)
            (if (solved? bd)
                (cons bd empty)
                (solve--lob (next-boards bd))))

          (define (solve--lob lob)
            (cond [(empty? lob) empty]
                  [else
                   (append (solve--bd (first lob))
                           (solve--lob (rest lob)))]))

          (define (keep-valid bd)
            (filter valid? bd))

          (define (valid? bd)
            (= 5 (length (remove-all "O" bd))))]

    (keep-valid (solve--bd bd))))

 
  
;; Board -> Boolean
;; Produce true if the board is solved (i.e. no false is present on the board)
(check-expect (solved? B0) false)
(check-expect (solved? B3) false)
(check-expect (solved? B4) true)

(define (solved? bd)
  (andmap string? bd))


;; Board -> (listof Board)
;; Produce a list of boards, with one more field filled with "X" or "0"
(check-expect (next-boards B0) (list
                                (list   "X"  false false
                                        false false false
                                        false false false)
                                 
                                (list   "O"  false false
                                        false false false
                                        false false false)))

(check-expect (next-boards B3) (list
                                (list "X" "O" "X"       
                                      "O" "O" "X"
                                      "X" "X" false)

                                (list "X" "O" "X"       
                                      "O" "O" "O"
                                      "X" "X" false)))
 
(define (next-boards bd)
  (make-boards bd (find-blank bd)))

 
;; Board -> Number [0, 8]
;; Produce the index of the next blank square on the boards (i.e. next false value in the list)
;; ASSUME: There always is a blank square on the board
(check-expect (find-blank B0) 0)
(check-expect (find-blank B2) 7)

(define (find-blank bd)
  (cond [(false? (first bd)) 0]
        [else (+ 1 (find-blank (rest bd)))]))

;; Board Index -> (listof Board)
;; Produce a list of 2 boards, one with "X" at given index, the other with "0" at given index
;; No need for tests here

(define (make-boards bd i) (list
                            (fill-square bd i "X")
                            (fill-square bd i "O")))


 
 
