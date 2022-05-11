#lang racket

(define mpr (mcons 1 (mcons #t "hi")))
mpr ; (mcons 1 (mcons #t "hi"))
(mcar mpr) ;1
(mcdr mpr) ; (mcons #t "hi")
(mcar (mcdr mpr)) ; #t
(set-mcdr! mpr 47)
mpr ; (mcons 1 47)
(set-mcdr! mpr (mcons #t "hi")) ; set it back.
mpr ;
