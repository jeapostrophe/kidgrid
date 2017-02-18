#lang racket/base
(require racket/match)

;; Output = Top,Bottom,Answer

(define (num . a)
  (match a
    [(list hi lo)
     (+ (* 10 hi) lo)]
    [(list hi mi lo)
     (+ (* 100 hi)
        (* 10 mi)
        lo)]))

(define (random* x)
  (if (zero? x) x (random x)))

(module+ main
  ;; No borrowing (2 digits)
  (for ([i (in-range 25)])
    ;; xy - ab
    (define x (+ 1 (random 8)))
    (define y (random 10))
    (define a (add1 (random* (sub1 x))))
    (define b (random* y))
    (printf "~a~a,~a~a,~a\n"
            x y a b
            (- (num x y)
               (num a b))))
  ;; Borrowing (2 digits)
  (for ([i (in-range 50)])
    ;; xy - ab
    (define x (+ 2 (random 7)))
    (define y (random 10))
    (define a (add1 (random* (sub1 x))))
    (define b (+ y (random* (- 10 y))))
    (printf "~a~a,~a~a,~a\n"
            x y a b
            (- (num x y)
               (num a b))))
  ;; Borrowing (3 digits)
  (for ([i (in-range 25)])
    ;; xyz - abc
    (define x (+ 2 (random 7)))
    (define y (random 10))
    (define z (random 10))
    (define a (add1 (random* (sub1 x))))
    (define b (+ y (random* (- 10 y))))
    (define c (+ z (random* (- 10 z))))
    (printf "~a~a~a,~a~a~a,~a\n"
            x y z
            a b c
            (- (num x y z)
               (num a b c)))))
