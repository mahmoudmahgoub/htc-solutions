;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tail-recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; (listof Number_ -> Number
;; sum all the numbers in the list
(check-expect (sum empty) 0)
(check-expect (sum (list 2 4 5)) 11)

(define (sum lon0)
  (local [(define (sum lon acc)
            (cond [(empty? lon) acc]
                  [else
                   (sum (rest lon) (+ acc (first lon)))]))]

    (sum lon0 0)))
