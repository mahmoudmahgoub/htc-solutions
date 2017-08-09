;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ta-solver) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/list) ;gets list-ref, take and drop


; PROBLEM 1:
; 
; Consider a social network similar to Twitter called Chirper. Each user has a name, a note about
; whether or not they are a verified user, and follows some number of people. 
; 
; Design a data definition for Chirper, including a template that is tail recursive and avoids 
; cycles. 
; 
; Then design a function called most-followers which determines which user in a Chirper Network is 
; followed by the most people.


(define-struct user (name verified follows))
;; User is (make-user String Boolean (listof Users))
;; interp: A user with a name, info about being verified or not and a list of users he/she follows

(define U1 (make-user "Jack" true (list (make-user "Bob" false empty))))

(define U2
  (shared ((-J- (make-user "Jack" true (list -B-)))
           (-B- (make-user "Bob" false (list -J-))))
    -J-))

(define U3
  (shared ((-J- (make-user "Jack"   true  (list -B-)))
           (-B- (make-user "Bob"    false (list -C- -E- -F-)))
           (-C- (make-user "Celine" true  (list -G-)))
           (-E- (make-user "Eugene" true  (list -J- -B-)))
           (-F- (make-user "Feick"  true  (list -B-)))
           (-G- (make-user "Glenn"  true  (list))))
    -J-))

 
;; function template

(define (fn-for-chirp u0)
  ;; todo is (listof User); worklist accumulator
  ;; visited is (listof String); context preserving  accumulator, list of names of users already seen
  (local [(define (fn-for-user u todo visited)
            (if (member (user-name u) visited)
                (fn-for-lou todo visited)
                (fn-for-lou (append (user-follows u) todo)
                            (cons (user-name u) visited))))

          (define (fn-for-lou lou todo visited)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-user (first todo)
                                (rest todo)
                                visited)]))]

    (fn-for-user u0 empty empty)))


;; User -> User
;; Produce the user with most followers in the graph
(check-expect (most-followers U1) (make-user "Bob" false empty))
(check-expect (most-followers U2) U2)
(check-expect (most-followers U3) (shared ((-J- (make-user "Jack"   true  (list -B-)))
                                           (-B- (make-user "Bob"    false (list -C- -E- -F-)))
                                           (-C- (make-user "Celine" true  (list -G-)))
                                           (-E- (make-user "Eugene" true  (list -J- -B-)))
                                           (-F- (make-user "Feick"  true  (list -B-)))
                                           (-G- (make-user "Glenn"  true  (list))))
                                    -B-))

(define (most-followers u0)
  ;; todo is (listof User); worklist accumulator
  ;; visited is (listof String); context preserving  accumulator, list of names of users already seen
  ;; rsf is (listof ResultsSoFarEntry); results so far acc, counts how many followers each user has
  (local [(define-struct rsfe (u n))
          ;; ResultsSoFarEntry is (make-rsfe (User Natural))
          ;; interp: number of people following that user seen so far

          (define (fn-for-user u todo visited rsf)
            (if (member (user-name u) visited)
                (fn-for-lou todo visited rsf)
                (fn-for-lou (append (user-follows u) todo)
                            (cons (user-name u) visited)
                            (add-user u rsf))))

          (define (fn-for-lou todo visited rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-user (first todo)
                                (rest todo)
                                visited
                                rsf)]))

          (define (add-user u rsf)
            (foldr add-followers rsf (user-follows u)))

          (define (add-followers u lorsfe)
            (cond [(empty? lorsfe) (list (make-rsfe u 1))]
                  [else
                   (if (string=? (user-name u) (user-name (rsfe-u (first lorsfe))))
                       (cons (make-rsfe u (add1 (rsfe-n (first lorsfe)))) (rest lorsfe))
                       (cons (first lorsfe) (add-followers u (rest lorsfe))))]))

          (define (find-max rsf)
            (rsfe-u
             (foldr
              (λ (e1 e2) (if (> (rsfe-n e2) (rsfe-n e1))
                             e2
                             e1))
              (first rsf)
              (rest rsf))))]

    (find-max (fn-for-user u0 empty empty empty))))





; PROBLEM 2:
; 
; In UBC's version of How to Code, there are often more than 800 students taking 
; the course in any given semester, meaning there are often over 40 Teaching Assistants. 
; 
; Designing a schedule for them by hand is hard work - luckily we've learned enough now to write 
; a program to do it for us! 
; 
; Below are some data definitions for a simplified version of a TA schedule. There are some 
; number of slots that must be filled, each represented by a natural number. Each TA is 
; available for some of these slots, and has a maximum number of shifts they can work. 
; 
; Design a search program that consumes a list of TAs and a list of Slots, and produces one
; valid schedule where each Slot is assigned to a TA, and no TA is working more than their 
; maximum shifts. If no such schedules exist, produce false. 
;
; You should supplement the given check-expects and remember to follow the recipe!





;; Slot is Natural
;; interp. each TA slot has a number, is the same length, and none overlap

;; Blank is false
;; interp. Empty slot in the schedule
(define B false)

(define-struct ta (name max avail))
;; TA is (make-ta String Natural (listof Slot))
;; interp. the TA's name, number of slots they can work, and slots they're available for

(define SOBA (make-ta "Soba" 2 (list 1 3)))
(define UDON (make-ta "Udon" 1 (list 3 4)))
(define RAMEN (make-ta "Ramen" 1 (list 2)))

(define NOODLE-TAs (list SOBA UDON RAMEN))



(define-struct assignment (ta slot))
;; Assignment is (make-assignment TA Slot)
;; interp. the TA is assigned to work the slot

;; Schedule is (listof Assignment)


;; ============================= FUNCTIONS


;; (listof TA) (listof Slot) -> Schedule or false
;; produce valid schedule given TAs and Slots; false if impossible

(check-expect (schedule-tas empty empty) empty)
(check-expect (schedule-tas empty (list 1 2)) false)
(check-expect (schedule-tas (list SOBA) empty) empty)

(check-expect (schedule-tas (list SOBA) (list 1)) (list (make-assignment SOBA 1)))
(check-expect (schedule-tas (list SOBA) (list 2)) false)
(check-expect (schedule-tas (list SOBA) (list 1 3)) (list (make-assignment SOBA 3)
                                                          (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4)) 
              (list
               (make-assignment UDON 4)
               (make-assignment SOBA 3)
               (make-assignment RAMEN 2)
               (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4 5)) false)


(define (schedule-tas tas slots)
  (cond [(empty? slots) empty]
        [(empty? tas) false]
        [else
         (local [(define (fn-for-schedule todo schedule)
                   ;; Schedule is result so far accumulator; What schedule we have built up to this point
                   (if (empty? todo)
                       schedule
                       (fn-for-los (rest todo) (next-schedules (first todo) tas schedule))))

                 (define (fn-for-los todo los)
                   (cond [(empty? los) false]
                         [else
                          (local [(define try (fn-for-schedule todo (first los)))]
                            (if (not (false? try))
                                try
                                (fn-for-los todo (rest los))))]))]

           (fn-for-schedule slots empty))]))




;; Slots (listof TA) Schedule -> (listof Schedule)
;; produce a list of valid next schedules
(check-expect (next-schedules 1 NOODLE-TAs empty) (list (list (make-assignment SOBA 1))))
(check-expect (next-schedules 2 NOODLE-TAs (list (make-assignment SOBA 1))) (list (list (make-assignment RAMEN 2) (make-assignment SOBA 1))))

(define (next-schedules slot tas schedule)
  (keep-valid tas (create-schedules slot tas schedule)))


;; Slot (listof TA) Schedule -> (listof Schedule)
;; produce a list of next schdules by inserting each TA that can take given Slot
(check-expect (create-schedules 1 NOODLE-TAs empty) (list
                                                     (list (make-assignment SOBA 1))))

(check-expect (create-schedules 3 NOODLE-TAs (list (make-assignment RAMEN 2) (make-assignment SOBA 1)))
              (list
               (list (make-assignment SOBA 3) (make-assignment RAMEN 2) (make-assignment SOBA 1))
               (list (make-assignment UDON 3) (make-assignment RAMEN 2) (make-assignment SOBA 1))))

(define (create-schedules slot tas schedule)
  (cond [(empty? tas) empty]
        [else
         (if (member? slot (ta-avail (first tas)))
             (cons (cons (make-assignment (first tas) slot) schedule)
                   (create-schedules slot (rest tas) schedule))
             (create-schedules slot (rest tas) schedule))]))


;; (listof Schedule) -> (listof Schedule)
;; filter the list by removing those schedules, in which any TA has assigned more Slots then they can take
(check-expect (keep-valid NOODLE-TAs (list
                                      (list (make-assignment SOBA 1)
                                            (make-assignment SOBA 3))))
              (list
                                      (list (make-assignment SOBA 1)
                                            (make-assignment SOBA 3))))
(check-expect (keep-valid NOODLE-TAs (list
                                      (list (make-assignment SOBA 1)
                                            (make-assignment SOBA 3)
                                            (make-assignment SOBA 2))))
              empty)

(define (keep-valid tas los)
  (local [(define (valid? schedule)
            (local [(define (compare ta)
                      (if (< (ta-max ta) (count ta schedule))
                          false
                          true))
                    
                    (define (count ta schedule)
                      (local [(define (counter as n)
                                (if (equal? (assignment-ta as) ta)
                                    (add1 n)
                                    n))]

                        (foldr counter 0 schedule)))]

              (andmap compare tas)))]
    
    (filter valid? los)))




