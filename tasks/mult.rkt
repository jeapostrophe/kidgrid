#lang racket/base

;; Output = Left,Right,Answer

(module+ main
  (for* ([x (in-range 11)]
         [y (in-range 11)])
    (printf "~a,~a,~a\n"
            x y (* x y))))
