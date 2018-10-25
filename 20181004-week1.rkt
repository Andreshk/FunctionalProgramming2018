#lang racket
(define x 5)

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (fact* n)
  (cond [(or (not (integer? n))
             (< n 0)) #f]
        [else (fact n)]))

(define (fib n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (fib (- n 1))
                 (fib (- n 2)))]))

(define (sqr x) (* x x))
(define (f x y)
  (or (and (< y 0) (<= (abs x) 1))
      (and (>= y 0) (<= (+ (sqr x) (sqr y)) 4))))
