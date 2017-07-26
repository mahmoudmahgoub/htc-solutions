;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname dropn) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; dropn-starter.rkt

; PROBLEM:
;
; Design a function that consumes a list of elements lox and a natural number
; n and produces the list formed by dropping every nth element from lox.
;
; (dropn (list 1 2 3 4 5 6 7) 2) should produce (list 1 2 4 5 7)


;; (listof X) Natural -> (listof X)
;; Produce a new list by droping every nth (Natural) element from a given list
(check-expect (dropn empty 999) empty)
(check-expect (dropn (list 1 2 3 4 5 6 7) 2) (list 1 2 4 5 7))
(check-expect (dropn (list 1 2 3 4 5 6 7) 1) (list 1 3 5 7))

(define (dropn lox0 n)
  (local [(define (drop lox acc)
            (cond [(empty? lox) empty]
                  [else
                   (if  (= 0 acc)
                        (drop (rest lox) n)
                        (cons (first lox)  (drop (rest lox) (- acc 1))))]))]

(drop lox0 n))) 