;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname strictly-decreasing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; strictly-decreasing-starter.rkt

;PROBLEM:
;
;Design a function that consumes a list of numbers and produces true if the 
;numbers in lon are strictly decreasing. You may assume that the list has at 
;least two elements.


;; (listof Number) -> Boolean
;; produce true if items in the list are strictly decreasing
;; ASSUME: List is at least 2 elements long
(check-expect (decreasing? (list 3 2 1)) true)
(check-expect (decreasing? (list 3 2 2)) false)
(check-expect (decreasing? (list 10 4 1)) false)
(check-expect (decreasing? (list 3 10 1)) false)

(define (decreasing? lon0)
  (local [(define (dec? lon acc)
            (cond [(empty? lon) true]
                  [else
                   (if  (= acc (add1 (first lon)))
                        (dec? (rest lon) (first lon))
                        false)]))]

    (dec? lon0 (add1 (first lon0)))))
