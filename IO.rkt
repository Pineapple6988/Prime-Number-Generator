#lang lazy
(require "IOStream.rkt")
(require "Gen.rkt")
;;Leaderboard owowowo
(define Nats (cons 0 (map (λ(x) (+ x 1)) Nats))) ;;start from 0
(define Nat (cons 1 (map (λ(x) (+ x 1)) Nat)))   ;;start from 1

(define (Kill k s)
  (Gen
   s  ;;stream input
   0  ;;0 acc
   (lambda (stream state cont)  ;step
     (cond
       [(empty? stream) empty]
       [(integer? (/ (car stream) k)) (cont (cdr stream) 0 empty)] ;if divisible, return empty
       [else (cont (cdr stream) 0 (list (car stream)))]))))        ;else, put into outstream
;(outstream (Kill 3 (take 5 Nats))) ;; outputs 1 2 4
;(outstream (take 5 (Kill 2 Nats))) ;; outputs 1 3 5 7 9 
;(outstream (Kill 5 '(10 15 20 31))) ;; outputs 31

(define primes
  (Gen
   Nat      ;;input Nat
   2
   (lambda (stream state cont)  ;step
     (cond
       [(not (prime? (car stream))) (cont (cdr stream) 0 empty)] ;if divisible, return empty
       [else (cont (cdr stream) 0 (list (car stream)))]))))
;;;;;;;;;;;;;;;;;;;;; prime? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (generate first done? step final)
 (if (done? first) (final first) (generate (step first) done? step final)))
(define (prime? n)
  (generate
   (list 2 n 0)   ;;(list a b c) => a = increasing int, b = n, c = acc ;if acc remains 0, then prime
   (lambda (st)(> (car st)(sqrt (cadr st))))  ;;quit when 1st parameter > sqrtn
   (lambda (st)
     (cond
       [(integer? (/ (cadr st) (car st)))(list (add1 (car st))(cadr st)(add1 (caddr st)))]  ;;if divisible, then add1 to 3rd parameter
       [else (list (add1 (car st))(cadr st)(caddr st))]))   ;;if not divisible, keep normal 
   (lambda (st)(if (or (= n 0) (= n 1) (> (caddr st) 0)) false true))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define twinprimes-slow ;;1433 in 2 minutes
  (Gen
   primes      ;;input primes
   2
   (lambda (stream state cont)  ;step
     (cond
       [(= (- (cadr stream) (car stream)) 2) (cont (cdr stream) 0 (list (list (car stream) (cadr stream))))] ;if divisible, return empty
       [else (cont (cdr stream) 0 empty)]))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Nat-odd (cons 3 (map (λ(x) (+ x 2)) Nat-odd)))
(define Squares_sub1 (cons 3 (map (λ(x) (sub1 (expt (+ x 1) 2))) Nat-odd))) ;;squares of odd -1

;(outstream (take 5 Squares_sub1))
;;;;;;;;;;;;;;;;;;;fast prime?;;;;;;;;;;;;;
(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp) (remainder (expt (expmod base (/ exp 2) m) 2) m)]
        [else (remainder (* base (expmod base (- exp 1) m)) m)]))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond [(= times 0) true]
        [(fermat-test n) (fast-prime? n (- times 1))] ;;rng not big enuff
        [else false]))
(define (fast-prime?? n)
  (cond [(fermat-test n) (fast-prime?? n)] ;;rng not big enuff
        [else false]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define twinprimes-fast  ;;all twinprimes are (x)(x+2)=a^2-1
  (Gen
   Squares_sub1      ;;input Squares_sub1
   0
   (lambda (stream state cont)  ;step
     (cond
       [(and (fast-prime?? (- (car stream) 1)) (fast-prime?? (+ (car stream) 1))) (cont (cdr stream) 0 (list (list (- (car stream) 1) (+ (car stream) 1))))] ;if divisible, return empty
       [else (cont (cdr stream) 0 empty)]))))
;(outstream (take 5 twinprimes-fast))