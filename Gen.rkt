#lang lazy

(provide Gen)

(require "IOStream.rkt")

(define (Gen inp state step)
  (define (cont inp state out)
    (append out (Gen inp state step)))
  (step inp state cont))
