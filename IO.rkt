#lang lazy
(require "IOStream.rkt")
(require "Gen.rkt")

(define Nat (cons 1 (map (λ(x) (+ x 1)) Nat)))   ;;start from 1

(define primes
  (Gen
   Nat
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
(define twinprimes
  (Gen
   primes
   2
   (lambda (stream state cont)
     (cond
       [(= (- (cadr stream) (car stream)) 2) (cont (cdr stream) 0 (list (list (car stream) (cadr stream))))] ;if divisible, return empty
       [else (cont (cdr stream) 0 empty)]))))
       
;; (outstream primes)
;; (outstream twin-primes)
       
