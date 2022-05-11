; Programming Language, Dan Grossman, October-November 2021
; Section 5; Rachet Definitions, Functions, Conditionals
#lang racket

(provide (all-defined-out))

(define x 3); val x = 3
(define y (+ x 2)) ; + is a function, call it here

(define cube1
  (lambda (x)
    (* x (* x x))
    )
  )

(define cube2
  (lambda (x)
    (* x x x)
    )
  )

(define (cube3 x)
  (* x x x)
)