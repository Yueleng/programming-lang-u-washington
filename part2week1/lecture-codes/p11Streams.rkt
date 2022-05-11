#lang racket

(provide (all-defined-out))

; streams

; '(next-answer . next-thunk)'
; 1 1 1 1 1 1 1 1 ...
(define ones (lambda () (cons 1 ones)))

; 
(define nats
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define powers-of-two
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))


; (define ones-really-bad (cons 1 ones-really-bad))
; (define ones-bad (lambda () cons 1 (ones-bad)))
; (define (ones-bad)(cons 1 (ones-bad)))
; (define ones (lambda () (cons 1 ones)))
; (define (ones)(cons 1 ones))

; (define (stream-maker fn arg) ...)
; (define nats (stream-maker + 1))
; (define powers-of-two (stream-maker * 2))