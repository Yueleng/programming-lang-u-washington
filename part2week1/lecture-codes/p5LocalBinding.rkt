#lang racket

(provide (all-defined-out))

(define (max-of-list xs)
  (cond [(null? xs) (error "max-of-list given empty list")]
        [(null? (cdr xs)) (car xs)]
        [#t (let ([tlans (max-of-list (cdr xs))])
              (if (> tlans (car xs))
                  tlans
                  (car xs)))]))


; let
; the expressions are all evaluated in the env
; from before the let-expression
(define (silly-double x)
  (let ([x (+ x 3)]
        [y (+ x 2)])
       (+ x y - 5)))
           

; let*
(define (silly-double2 x)
  (let* ([x (+ x 3)]
         [y (+ x 2)])
    (+ x y -8)))




; letrec
(define (silly-triple x)
  (letrec ([y (+ x 2)]
           [f (lambda(z) (+ z y w x))]
           [w (+ x 7)])
    (f -9)))


; more letrec

; do not use later bindings except inside functions
(define (bad-letrec x)
  (letrec ([y z]
            [z 13])
     (if x y z)))

; letrec is ideal for recursion (including mutual recursion)

(define (silly-mod2 x)
  (letrec
   ([even? (lambda(x) (if (zero? x) #t (odd? (- x 1))))]
    [odd? (lambda(x) (if (zero? x) #f (even? (- x 1))))])
   (if (even? x) 0 1)))


; local defines
(define (silly-mod2-define x)
  (define (even? x) (if (zero? x) #t (odd? (- x 1))))
  (define (odd? x) (if (zero? x) #f (even? (- x 1))))
  (if (even? x) 0 1))