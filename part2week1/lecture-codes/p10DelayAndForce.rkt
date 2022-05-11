#lang racket

(provide (all-defined-out))

(define (slow-add x y)
  (letrec ([slow-id (lambda (y z)
                      (if (= 0 z)
                      y
                      (slow-id y (- z 1))))])
    (+ (slow-id x 50000000) y)))

(define (my-mult x y-thunk) ;; assumes x is >= 0
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult (- x 1) y-thunk))]))


; #f in car means cdr is unevaluated thunk
(define (my-delay th)
  (mcons #f th))


; really a one-of-type: thunk or result-of-thunk
(define (my-force p)
  (if (mcar p)
      (mcdr p)
      (begin (set-mcar! p #t)
             (set-mcdr! p ((mcdr p)))
             (mcdr p))))

(define _thunk (let ([p (my-delay (lambda () (slow-add 3 4)))])
              (lambda () (my-force p))))

; the best solution so far
(my-mult 50 _thunk)