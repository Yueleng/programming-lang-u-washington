#lang racket

(provide (all-defined-out))

; this is silly addtion function that purposely runs
; slows for demonstrate purposes
(define (slow-add x y)
  (letrec ([slow-id (lambda (y z)
                      (if (= 0 z)
                      y
                      (slow-id y (- z 1))))])
    (+ (slow-id x 50000000) y)))

; multiple x and result of y-thunk, calling y-thunk x times
(define (my-mult x y-thunk) ;; assumes x is >= 0
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult (- x 1) y-thunk))]))


; twice amount of time.
; (my-mult 2 (lambda () (slow-add 3 4)))


; always the same amount of time
; (my-mult 0 (let ([x (slow-add 3 4)]) (lambda () x)))
; (my-mult 20 (let ([x (slow-add 3 4)]) (lambda () x)))