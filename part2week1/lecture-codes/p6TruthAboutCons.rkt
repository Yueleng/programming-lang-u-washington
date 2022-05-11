#lang racket
(provide (all-defined-out))

(define pr (cons 1 (cons #t "hi")))
(define pr2 ( cons 2 (cons 1 (cons #t "hi"))))
(define lst (cons 1 (cons #t (cons "hi" null))))

pr ; '(1 #t . "hi")
(cdr (cdr pr)) ; "hi"
(cdr (cdr lst)); '("hi")
(car (cdr (cdr lst))) ; "hi"
(caddr lst) ; "hi" ; (define (caddr x) (car (cdr (cdr x))))
(list? pr) ; #f
(list? lst) ; #t
(pair? pr); #t
(and (pair? lst) (list? lst)) ; #t, i.e. list must be a pair, but not vice versa.