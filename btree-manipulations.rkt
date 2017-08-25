;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname btree-manipulations) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct node (val left right))
;; BT is one of:
;; - false
;; - (make-node Natural Node Node)
;; interp: Just a binary search tree
(define N1 (make-node 1 false false))
(define N2 (make-node 2 N1 false))
(define N4 (make-node 4 false false))
(define N3 (make-node 3 N2 N4))

(define N10 (make-node 10 false false))
(define N11 (make-node 11 N10 false))
(define N12 (make-node 12 N11 false))
(define N6 (make-node 6 false false))
(define N8 (make-node 8 false false))
(define N7 (make-node 7 N6 N8))
(define N9 (make-node 9 N7 N12))

(define N5 (make-node 5 N3 N9))



;; BT -> BT
;; produce a reversed BT
(check-expect (rev N1) N1)
(check-expect (rev N2) (make-node 2 false N1))
(check-expect (rev N3) (make-node 3 N4 (make-node 2 false N1)))
(check-expect (rev N5) (make-node 5
                                  (make-node 9
                                             (make-node 12 false (make-node 11 false (make-node 10 false false)))
                                             (make-node 7 (make-node 8 false false) (make-node 6 false false)))
                                  (make-node 3 N4 (make-node 2 false N1))))

(define (rev n)
  (if (false? n)
      false
      (make-node (node-val n) (rev (node-right n)) (rev (node-left n)))))


;; BT -> BT
;; produce an inverted BT
(check-expect (invert N1) N1)
(check-expect (invert N3) (make-node 1 false
                                     (make-node 2
                                                (make-node 4 false false)
                                                (make-node 3 false false))))
                                                           

(check-expect (invert N5) (make-node 1 false
                                       (make-node 2 (make-node 4 false false)
                                                    (make-node 3 N9 (make-node 5 false false)))))

(define (invert n0)
  (local [(define (inv n rsf)
            (if (false? (node-left n))
                rsf
                (inv (node-left n) (make-node
                                          (node-val (node-left n))
                                          (node-right n)
                                          rsf))))]

    (inv n0 (make-node (node-val n0) false false)))) 
                                     





