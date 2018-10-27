#lang racket
(define dx 0.0000001)
;(define (derive f x)
;  (/ (- (f (+ x dx)) (f x)) dx))
(define (derive f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (sq x) (* x x))

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (!! n)
  (accumulate * 1
              (if (even? n) 2 1) n
              (lambda (i) i) (lambda (i) (+ i 2))))

(define (nchk n k)
  (define (fact n) (accumulate * 1 1 n id 1+))
  (/ (fact n) (fact k) (fact (- n k))))

(define (nchk* n k)
  (accumulate * 1
              1 k
              (lambda (i) (/ (+ (- n k) i) i)) ; (n-k+i)/i
              1+))

(define (id x) x) ; (define id (lambda (x) x))
(define (1+ x) (+ x 1))

(define (2^ n)
  ;(accumulate * 1
  ;            1 n
  ;            (lambda (i) 2) 1+))
  (accumulate + 0
              0 n
              (lambda (i) (nchk* n i))
              1+))

(define (divisors-sum n)
  (accumulate + 0
              1 n
              (lambda (i) (if (zero? (remainder n i))
                              i
                              0))
              1+))

(define (filter-accum p? op nv a b term next)
  (cond [(> a b) nv]
        [(p? a) (op (term a)
                    (filter-accum p? op nv (next a)
                                  b term next))]
        [else (filter-accum p? op nv (next a)
                            b term next)]))

(define (divisors-sum* n)
  (filter-accum (lambda (i) (zero? (remainder n i)))
                + 0
                1 n
                id 1+))

(define (count p? a b)
;  (filter-accum p?
;                + 0
;                a b
;                (lambda (i) 1)
;                1+))
  (accumulate + 0
              a b
              (lambda (i) (if (p? i) 1 0))
              1+))

(define (all? p? a b)
  (accumulate (lambda (x y) (and x y))
              #t
              a b
              p? 1+))

(define (any? p? a b)
  (not (all? (lambda (x) (not (p? x))) a b)))

(define (divides? n)
  (lambda (i) (zero? (remainder n i))))
(define (prime? n)
  (not (or (= n 1)
           (any? (divides? n) 2 (- n 1)))))
