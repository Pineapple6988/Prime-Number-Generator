#lang lazy

(provide Gen)

(require "IOStream.rkt")

;; (Gen inp state step) creates a lazy list
;;   inp is the overall input (typically but not necessarily a list)
;;   state is the internal state
;;   (step inp state cont) returns a list, either directly, or by invoking cont
;;      (cont newinp newstate newoutput) is a "continuation" that appends the elements of new
;;       output to the beginning of the list returned by (Gen newinp newstate step)


(define (Gen inp state step)
  (define (cont inp state out)
    (append out (Gen inp state step)))
  (step inp state cont))
