#lang racket


(provide (all-defined-out))

;basic definitions
(define s "hello") ; this is another comment

(define x 3) ; val x = 3
(define y 4) ; val y = 4
(define y1 (+ x 2)) ; + is a function, call it here.


; define a function
(define cube1
  (lambda (x)
    (* x (* x x))))

; short hand ;takes any number of arguments
(define cube2
  (lambda (x)
    (* x x x)))

(define (cube3 x)
  (* x x x))


; recursive function
(define (pow1 x y)
  (if (= y 0)
      1
      (* x (pow1 x (- y 1)))))


; currying
(define pow2
  (lambda (x)
    (lambda (y)
      (pow1 x y))))

; 三的x次方
(define three-to-the
  (pow2 3))

; parathesis matter
(define sixteen (pow1 4 2))
(define _sixteen ((pow2 4) 2))

; function call syntax: (e0 e1 ... e2)