;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname max-exits-to) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

;; max-exits-to-starter.rkt

;PROBLEM:
;
;Using the following data definition, design a function that produces the room to which the greatest 
;number of other rooms have exits (in the case of a tie you can produce any of the rooms in the tie).

;; Data Definitions: 

(define-struct room (name exits))
;; Room is (make-room String (listof Room))
;; interp. the room's name, and list of rooms that the exits lead to

(define H1 (make-room "A" (list (make-room "B" empty))))

(define H2 
  (shared ((-0- (make-room "A" (list (make-room "B" (list -0-))))))
    -0-)) 

(define H3
  (shared ((-A- (make-room "A" (list -B-)))
           (-B- (make-room "B" (list -C-)))
           (-C- (make-room "C" (list -A-))))
    -A-))
           
(define H4
  (shared ((-A- (make-room "A" (list -B- -D-)))
           (-B- (make-room "B" (list -C- -E-)))
           (-C- (make-room "C" (list -B-)))
           (-D- (make-room "D" (list -E-)))
           (-E- (make-room "E" (list -F- -A-)))
           (-F- (make-room "F" (list))))
    -A-))

;; template: structural recursion, encapsulate w/ local, tail-recursive w/ worklist, 
;;           context-preserving accumulator what rooms have we already visited

(define (fn-for-house r0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  (local [(define (fn-for-room r todo visited) 
            (if (member (room-name r) visited)
                (fn-for-lor todo visited)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited)))) ; (... (room-name r))
          (define (fn-for-lor todo visited)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-room (first todo) 
                                (rest todo)
                                visited)]))]
    (fn-for-room r0 empty empty)))

 

;; Room -> Room
;; produce a room to which the greatest number of other rooms have exits
(check-expect (max-exits-to H1) (make-room "B" empty))
(check-expect (max-exits-to H2) H2)
(check-expect (max-exits-to H4) (shared ((-A- (make-room "A" (list -B- -D-)))
                                         (-B- (make-room "B" (list -C- -E-)))
                                         (-C- (make-room "C" (list -B-)))
                                         (-D- (make-room "D" (list -E-)))
                                         (-E- (make-room "E" (list -F- -A-)))
                                         (-F- (make-room "F" (list))))
                                  -B-))

(define (max-exits-to r0)
  (local [(define-struct rne (r n))
          ;; rne is (make-rne (Room Natural))
          ;; interp: number of exits to given room seen so far

          (define (fn-for-room r todo visited rsf) 
            (if (member (room-name r) visited)
                (fn-for-lor todo visited rsf)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited)
                            (add-room r rsf))))

          (define (fn-for-lor todo visited rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-room (first todo) 
                                (rest todo)
                                visited
                                rsf)]))

          (define (add-room r rsf)
            (foldr merge-room rsf (room-exits r)))

          (define (merge-room r lorne)
            (cond [(empty? lorne) (list (make-rne r 1))]
                  [else
                   (if (string=? (room-name r) (room-name (rne-r (first lorne))))
                       (cons (make-rne r (add1 (rne-n (first lorne)))) (rest lorne))
                       (cons (first lorne) (merge-room r (rest lorne))))]))

          (define (find-max rsf)
            (rne-r (foldr
                    (λ (n1 n2) (if (> (rne-n n2) (rne-n n1))
                                       n2
                                       n1))
                    (first rsf)
                    (rest rsf))))]

    (find-max (fn-for-room r0 empty empty empty))))


























