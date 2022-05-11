#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file
;; put your code below

(define (sequence low high stride)
	(if (> low high) 
	  null
	  (cons low (sequence (+ low stride) high stride))))


(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
		[(null? xs) (error "list-nth-mod: empty list")]
		[#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (letrec ([f (lambda (counter s)
				(let ([res (s)])
					(if (= counter n)
					  null
					  (cons (car res) (f (+ 1 counter) (cdr res))))))])
	(f 0 s)))


(define funny-number-stream
  (letrec ([f (lambda (x) (if (= (remainder x 5) 0)
							(cons (- x) (lambda () (f (+ x 1))))
							(cons x (lambda () (f (+ x 1))))))])
	(lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) (if (= x 0)
							(cons "dan.jpg" (lambda () (f 1)))
							(cons "dog.jpg" (lambda () (f 0)))))])
	(lambda () (f 0))))

(define (stream-add-zero s)
  (let ([res (s)])
	(lambda () (cons (cons 0 (car res)) (stream-add-zero (cdr res))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x)
				(cons (cons (list-nth-mod xs x) (list-nth-mod ys x)) (lambda () (f (+ x 1)))))])
	(lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (x)
				(cond [(vector-empty? vec) #f]
					  [(and (pair? (vector-ref vec x)) (equal? (car (vector-ref vec x)) v)) (vector-ref vec x)]
					  [(= x (- (vector-length vec) 1)) #f]
					  [#t (f (+ 1 x))]))])
	(f 0)))


(define (cached-assoc xs n)
  (letrec ([pos 0]
		   [cache (make-vector n #f)]
		   [f (lambda (x) 
				(let ([ans (assoc x (filter pair? (vector->list cache)))])
				  (if ans
					ans
					(let ([new-ans (assoc x xs)])
					  (if new-ans
						(if (< pos n)
						  (begin
							(vector-set! cache pos new-ans)
							(set! pos (+ 1 pos))
							new-ans)
						  (begin
							(vector-set! cache 0 new-ans)
							(set! pos 0)
							new-ans))
						#f)))))])
	f))

(define-syntax while-less
  (syntax-rules (do)
	[(while-less e1 do e2)
	 (let ([l e1]
		   [expr (lambda () e2)])
	   (letrec ([f (lambda ()
					 (if (< (expr) l)
					   (f)
					   #t))])
	   (f)))]))

