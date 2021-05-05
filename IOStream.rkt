#lang lazy

;; This module implements input and output streams as lazy lists

;; (outstream S) -- send S to output, one element per line
;; instream -- each element of instream is read from input

;; Example
;;
;; (outstream (map add1 instream))
;;
;; outputs 1 greater than every number in input

(provide instream outstream)

(define (make-instream)
  (define in (read))
  (cond
    [(eof-object? in) empty]
    [true (cons in (make-instream))]))

(define instream (make-instream))

(define (outstream s)
  (cond
    [(empty? s) (void)]
    [true
     (display (car s))
     (newline)
     (outstream (cdr s))]))