#lang lazy

(provide 
  Gen
  accum
  Gen
  trues
  falses
  stutter
  fibs
  accum
  less
  more
  ranseq
  Flatten)

(require "IOStream.rkt")

;; (Gen inp state step) creates a lazy list
;;   inp is the overall input (typically but not
;;                            necessarily a list)
;;   state is the internal state
;;   (step inp state cont) returns a list, either
;;      directly, or by invoking cont
;;      (cont newinp newstate newoutput)
;;          is a "continuation" that appends the
;;          elements of newoutput to the beginning
;;          of the list returned by
;;          (Gen newinp newstate step)

;; Examples.
;; Cumulative sum of a list
;; (define (accum s)
;;  (Gen
;;    s  ;; input list
;;    0  ;; initial accumulator
;;    (lambda(in state cont)
;;      (if (empty? in) empty
;;        (cont (cdr in)
;;              (+ state (car in))
;;              (list (+ state (car in))))))))
;;
;; Example usage:  (outstream (accum '(1 2 3 4)))
;; Output for example: 1 3 6 10
;;
;; Explanation:
;;   inp is the input list
;;   state is the sum so far
;;   if inp is not empty, the step function
;;     removes one element from inp
;;     adds the element to state
;;     "outputs" the element using cont
;;     note that the "output" must be a list

;; See other examples below     

(define (Gen inp state step)
  (define (cont inp state out)
    (append out (Gen inp state step)))
  (step inp state cont))

;; No need for Gen for trivial stuff
(define trues (cons true trues))

;; But if you insist, here is a list of true
(define Trues
  (Gen
   0 ;; just a placeholder, never used
   0 ;; just another placeholder, never used
   (lambda (inp state cont)
     (cont 0 0 (list true)))))

(define falses (map not trues))

;(display (take 5 trues))

;; There is no need to redefine builtin functions
;; But if you insist:
(define (mymap f s)
  (Gen
   s        ;; input list
   0        ;; placeholder, not used
   (lambda (stream state cont)
     (if (empty? stream) empty
       (cont (cdr stream) 0 (list (f (car stream))))))))

;; a test expression
;; (display (take 5 (mymap not trues)))

;; stutter repeats every element in s
(define (stutter s)
  (Gen
   s  ;; input list
   0  ;; placeholder, not used
   (lambda (stream state cont)
     (cond
       [(empty? stream) empty]
       [true
          (cont
           (cdr stream)  ;; discard head of list
           0
           (list (car stream) (car stream)))]))))
               ;; output head of list twice

;; (outstream (stutter '(1 2 3)))

(define fibs
  (Gen
   0   ;; current Fibonacci number
   1   ;; next Fibonacci number
   (lambda (this next cont)
     (cont next ;; next is new current
           (+ this next) ;; new next
           (list this))))) ;; output current

;(display (take 10 fibs))

;; Cumulative sum; see comments above
(define (accum s)
  (Gen
   s  ;; input list
   0  ;; cumulative sum
   (lambda (in state cont)
     (if (empty? in) empty
       (cont (cdr in) (+ state (car in)) (list (+ state (car in))))))))

;; Still no need to reinvent builtins; but here is map with two lists
(define (map2 f s t)
  (Gen
   s   ;; first list
   t   ;; second list
   (lambda (inp1 inp2 cont)
     (cond
       [(empty? inp1) empty]
       [(empty? inp2) empty]
       [true
        (cont
         (cdr inp1)  ;; consume head of 1st list
         (cdr inp2)  ;; consume head of 2nd list
         (list (f (car inp1) (car inp2))))]))))

;; Convenient function for debugging
(define (o s)
         (outstream (take 5 s)))

;; "Pager" program that outputs s, prompting
;; user to continue or not after every 5 elements
;; Try it:  (less fibs)

(define (less s)
 (outstream
  (Gen
   s
   (cons 5 instream)
   (lambda (inp state cont)
     (cond
       [(zero? (car state))
        (cont
         inp
         (cons -1 (cdr state))
         (list "Continue?  (y/n)"))]
       [(= -1 (car state))
        (if (and (not (empty? (cdr state))) (eq? (cadr state) 'y))
            (cont inp (cons 5 (cddr state)) empty)
            empty)]
       [(empty? inp) empty]
       [true
        (cont
         (cdr inp)
         (cons (sub1 (car state)) (cdr state))
         (list (car inp)))])))))

;; A version of less parametrized by input and output
;; Try it:  (more fibs instream outstream)
;; Try it again:
;;  (more fibs (build-list 3 (lambda(x) 'y)) outstream)
;; And again:
;;  (more fibs instream (lambda(x)(outstream (stutter x))))

(define (more s instream outstream)
 (outstream
  (Gen
   s
   (cons 5 instream)
   (lambda (inp state cont)
     (cond
       [(zero? (car state))
        (cont
         inp
         (cons -1 (cdr state))
         (list "Continue?  (y/n)"))]
       [(= -1 (car state))
        (if (and (not (empty? (cdr state))) (eq? (cadr state) 'y))
            (cont inp (cons 5 (cddr state)) empty)
            empty)]
       [(empty? inp) empty]
       [true
        (cont
         (cdr inp)
         (cons (sub1 (car state)) (cdr state))
         (list (car inp)))])))))

;; A sequence of random numbers betwen 0 and n-1
;; Doesn't use Gen but could have!
(define (ranseq n) (cons (random n) (ranseq n)))

;; A sequence of random Rock-Paper-Scissors moves
(define (rps) (map (lambda(x) (list-ref '(rock paper scissors) x)) (ranseq 3)))

;; A relation showing what move wins in RPS
;; Note that this is *not* a total order!!!
(define
  (rps-beats a b)
  (or
   (and (eq? a 'rock) (eq? b 'scissors))
   (and (eq? a 'paper) (eq? b 'rock))
   (and (eq? a 'scissors) (eq? b 'paper))))

;; Determine who won
(define (rps-winner a b)
  (cond
    [(rps-beats a b) 'Player1]
    [(rps-beats b a) 'Player2]
    [true 'draw]))

;; An RPS game between a and b
(define (rps-game a b)
  (map rps-winner a b))

;; A strategy to cheat if opponents moves known
(define (rps-cheat opponent)
  (define (mymove other)
    (cond
      [(rps-beats 'rock other) 'rock]
      [(rps-beats 'paper other) 'paper]
      [true 'scissors]))
  (map mymove opponent))

;; RPS game with running history
;; shows a's move, b's move and who won
(define (playgame a b)
  (map list a b (map rps-winner a b)))

;; P1 and P2 are fair players
(define P1 (rps))
(define P2 (rps))

;; P3 always beats P1
(define P3 (rps-cheat P1))

;; P4 always beats P3
(define P4 (rps-cheat P3))

;; P5 always beats P6
;; but P6 always beats P5
;; impasse!
(define P5 (rps-cheat P6))
(define P6 (rps-cheat P5))

;; P7 and P8 don't cheat because they use
;; only *old* moves from P8 and P7
(define P7 (rps-cheat (cons 'rock P8)))
(define P8 (rps-cheat (cons 'rock (cons 'scissors P7))))

;; Create a history
;; for each position in list
;; (outstream (history '(rock paper scissors))) outputs
;;
;;   ()
;;   (rock)
;;   (paper rock)
;;   (scissors paper rock)

(define (history s)
  (define (helper s h)
    (cons h
      (cond
        [(empty? s) empty]
        [true (helper (cdr s) (cons (car s) h))])))
  (helper s empty))

;; No need to re-implement abstract list functions,
;; But flatten seems to be missing from Lazy Racket, so...
;; The invariant is (Flatten (append a b))
;; We need to make (size a) smaller without increasing
;; (size a) + (size b)
(define (Flatten s)
  (Gen
   s
   empty
   (lambda (a b cont)
     (cond
       [(pair? a)
        (if (empty? (cdr a))
             (cont (car a) b empty)
             (cont (car a) (cons (cdr a) b) empty))]
       [(not (empty? a))
        (cont b empty (list a))]
       [(empty? b) empty]
       [true
        (cont b empty empty)]))))

;; (outstream (Flatten '(1 ((2 (3) () 4 (5 6))))))
   
