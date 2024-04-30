(struct obj (fields methods))

; def assoc-m
(define (assoc-m v xs)
    (cond 
        [(null? xs) #f] ;if xs is null, then return false
        ; mcar: return the first ele of the mutable pairs
        [(equal? v (mcar (car xs))) (car xs)] ; if key match, return (car xs)
        [#t (assoc-m v (cdr xs))] ; iteratively call (cdr xs)
    )
)

(define (get obj fld)
    (let 
        ([pr (assoc-m fld (obj-fields obj))])
        (if pr
            ; Returns the second element of the mutable pair p.
            (mcdr pr)
            (error "field not found")
        )
    )
)

(define (set obj fld v)
    (let 
        ([pr (assoc-m fld (obj-fields obj))])
        (if pr
            (set-mcdr! pr v)
            (error "field not found")
        )
    )
)

(define (send obj msg . args)
    (let 
        ([pr (assoc msg (obj-methods obj))])
        (if pr
            ((cdr pr) obj args)
            (error ""method not found" msg)
        )
    )
)

(define (make-point _x _y)
  (obj 
    (list 
        (mcons 'x _x)
        (mcons 'y _y)
    )
    (list 
        (cons 'get-x (lambda (self args) (get self 'x)))
        (cons 'get-y (lambda (self args) (get self 'y)))
        (cons 'set-x (lambda (self args) (set self 'x (car args))))
        (cons 'set-y (lambda (self args) (set self 'y (car args))))
        (cons 'distToOrigin
            (lambda (self args)
                (let ([a (send self 'get-x)]
                      [b (send self 'get-y)])
                    (sqrt (+ (* a a) (* b b)))
                )
            )
        )
    )
  )
)


(define p1 (make-point 4 0))
(send p1 'get-x) ; 4
(send p1 'get-y) ; 0
(send p1 'disToOrigin) ; 4
(send p1 'set-y 3)
(send p1 'distToOrigin) ; 5

(define (make-color-point _x _y _c)
    (let ([pt (make-point _x _y)])
        (obj
            (cons (mcons 'color _c) (obj-fields pt))
            (append 
                (list 
                    (cons 'get-color (lambda (self args) (get self 'color)))
                    (cons 'set-color (lambda (self args) (set self 'color (car args))))
                )
                (obj-methods pt)
            )
        )
    )
)


(define (make-polar-point _r _th)
    (let ([pt (make-point #f #f)])
        (obj 
            (append 
                (list 
                    (mcons 'r _r)
                    (mcons 'theta _th)
                )
                (obj-fields pt)
            )
            (append
                (list
                    (cons 
                        'set-r-theta 
                        (lambda 
                            (self args)
                            (begin 
                                (set self 'r (car args))
                                (set self 'theta (cadr args))
                            )
                        )
                    )
                    (cons
                        'get-x
                        (lambda (self args)
                            (let 
                                (
                                    [r (get self 'r)]
                                    [theta (get self 'theta)]
                                )
                                (* r (cos theta))
                            )
                        )
                    )
                    (cons 
                        'get-y
                        (lambda (self args)
                            (let
                                (
                                    [r (get self 'r)]
                                    [theta (get self 'theta)]
                                )
                                (* r (sin theta))
                            )
                        )
                    )
                    (cons
                        'set-x
                        (lambda (self args)
                            (let* 
                                (
                                    [a (car args)]
                                    [b (send self 'get-y)]
                                    [theta (atan (/ b a))]
                                    [r (sqrt (+ (* a a) (* b b)))]
                                )
                                (send self 'set-r-theta r theta)
                            )
                        )
                    )
                    (cons
                        'set-y
                        (lambda (self args)
                            (let* 
                                (
                                    [b (car args)]
                                    [a (send self 'get-x)]
                                    [theta (atan (/ b a))]
                                    [r (sqrt (+ (* a a) (* b b)))]
                                )
                                (send self 'set-r-theta r theta)
                            )
                        )
                    )
                )
                (obj-methods pt)
            )
        )
    )
)