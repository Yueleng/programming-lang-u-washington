
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; Problem 1
(define (sequence low high stride)
  (if (> low high)
      empty
      (cons low (sequence (+ low stride) high stride))))


; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

; Problem 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(empty? xs) (error "list-nth-mod: empty list")]
        [(car (list-tail xs (remainder n (length xs))))]))

; Problem 4
(define (stream-for-n-steps s n)
    (if (= n 0)
        empty
        (let ([pr (s)])
          (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

; Problem 5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (let ([stream (lambda () (f (+ x 1)))])
                  (if (= 0 (modulo x 5))
                      (cons (* x -1) stream)
                      (cons x stream))))])
    (lambda () (f 1))))

; Problem 6
(define dan-then-dog
  (letrec ([f (lambda (x)
                (if (equal? x "dan.jpg")
                    (cons "dan.jpg" (lambda () (f "dog.jpg")))
                    (cons "dog.jpg" (lambda () (f "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

; Problem 7
(define (stream-add-zero s)
  (letrec ([f (lambda (s)
                (let ([pr (s)])
                  (cons (cons 0 (car pr))
                     (stream-add-zero (cdr pr)))))])
    (lambda () (f s))))

; Problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
              (cons (cons (list-nth-mod xs n)
                          (list-nth-mod ys n))
                    (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))
    
; Problem 9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (cond [(= n (vector-length vec)) #f]
                      [(not (pair? (vector-ref vec n))) (f (+ n 1))]
                      [(equal? (car (vector-ref vec n)) v) (vector-ref vec n)]
                      [#t (f (+ n 1))]))])
    (f 0)))

; Problem 10
(define (cached-assoc xs n)
  (letrec ([cache-slot 0]
           [memo (make-vector n #f)]
           [f (lambda (x)
                (let ([ans (vector-assoc x memo)])
                  (if ans
                      ans
                      (let ([new-ans (assoc x xs)])
                        (begin
                          (vector-set! memo cache-slot new-ans)
                          (set! cache-slot (remainder (+ cache-slot 1) n))
                          new-ans)))))])
    f))


; Problem 11
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do body)
     (let ([number e1])
       (letrec ([loop (lambda ()
                        (let ([result body])
                        (if (>= result number)
                            #t
                            (loop))))])
         (loop)))]))
