;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname skipn) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; skipn-starter.rkt

;PROBLEM:
;
;Design a function that consumes a list of elements lox and a natural number
;n and produces the list formed by including the first element of lox, then 
;skipping the next n elements, including an element, skipping the next n 
;and so on.
;
; (skipn (list "a" "b" "c" "d" "e" "f") 2) should produce (list "a" "d")

;; (lixtof X) Natural -> (listof X)
;; produce a list that takes one element of a given list, then skips n (Natural), takes one, skips n and so on...
(check-expect (skipn empty 666) empty)
(check-expect (skipn (list "a" "b" "c" "d" "e" "f") 2) (list "a" "d"))
(check-expect (skipn (list 1 2 3 4 5 6 7 8 9) 3) (list 1 5 9))

(define (skipn lox0 n)
  (local [(define (skip lox acc )
            (cond [(empty? lox) empty]
                  [else
                   (if  (= 0 acc)
                        (cons (first lox) (skip (rest lox)  n))
                        (skip (rest lox) (- acc 1)))]))]

    (skip lox0 0)))