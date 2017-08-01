;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname merge-sort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/list)


;; (listof Number) -> (listof Number)
;; produce a sorted list of those numbers in ascending order using merge sort
(check-expect (merge-sort empty) empty)
(check-expect (merge-sort (list 1)) (list 1))
(check-expect (merge-sort (list 3 2)) (list 2 3))
(check-expect (merge-sort (list 2 3)) (list 2 3))
(check-expect (merge-sort (list 2 3 10 7 6 14 5 3 2)) (list 2 2 3 3 5 6 7 10 14))

(define (merge-sort lon)
  (cond [(empty? lon) empty]
        [(empty? (rest lon)) lon]
        [else
         (merge
              (merge-sort (take lon (quotient (length lon) 2)))
              (merge-sort (drop lon (quotient (length lon) 2))))]))



;; (listof Number) (listof Number) -> (listof Number)
;; merge two lists into a single one, sorted in ascending order 
;; ASSUME: both given lists are sorted
(check-expect (merge empty empty) empty)
(check-expect (merge (list 1 2) empty) (list 1 2))
(check-expect (merge empty (list 1 2)) (list 1 2))
(check-expect (merge (list 1 2) (list 3 4)) (list 1 2 3 4))
(check-expect (merge (list 3 4) (list 1 2)) (list 1 2 3 4))
(check-expect (merge (list 1 3 5 7) (list 2 4 6 8)) (list 1 2 3 4 5 6 7 8))

(define (merge lon1 lon2)
  (cond [(empty? lon2) lon1]
        [(empty? lon1) lon2]
        [else
         (if (> (first lon1) (first lon2))
             (cons (first lon2) (merge lon1 (rest lon2)))
             (cons (first lon1) (merge (rest lon1) lon2)))]))


