;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
;; a variable, e.g., (var "foo")
;; variable usage -  (var "s") means variable named "s"
(struct var  (string) #:transparent)

;; a constant number, e.g., (int 17)
(struct int  (num)    #:transparent)

;; add two expressions
;; (add (int 3) (int 3))
(struct add  (e1 e2)  #:transparent)

;; if e1 > e2 then e3 else e4
;; (ifgreater (int 2) (int 1) (int 3) (int 4))
(struct ifgreater (e1 e2 e3 e4)    #:transparent)

;; a recursive(?) 1-argument function
;; (fun "doubleInt" "x" (* (int 2) (var "x")) )
(struct fun  (nameopt formal body) #:transparent)

;; function call
;; (call (fun "doubleInt" "x" (* (int 2) (var "x"))) (int "20") )
(struct call (funexp actual)       #:transparent)

;; a local binding (let var = e in body)
;; (mlet (var "y") (int 9) (fun "doubleInt" "x" (* (int 2) (var "x")) ) )
(struct mlet (var e body) #:transparent) 

;; make a new pair
;; (apair (int 2) (int 3))
(struct apair (e1 e2)     #:transparent) 

;; get first part of a pair
;; (fst (apair (int 2) (int 3)) )
(struct fst  (e)    #:transparent)


;; get second part of a pair
;; (snd (apair (int 2) (int 3)) )
(struct snd  (e)    #:transparent)

;; unit value -- good for ending a list
;; (aunit)
(struct aunit ()    #:transparent)

;; evaluate to 1 if e is unit else 0
;; (isaunit (aunit)) will evaluate to (int 1)
;; (isaunit (int 2)) will evaluate to (int 0)
(struct isaunit (e) #:transparent) 


;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
;; (
;;    closure
;;    (cons (cons "z" 7) null)
;;    (fun "doubleInt" "x" (* (var "z") (var "x")))
;; )
(struct closure (env fun) #:transparent)

;; Problem 1
;; Write a Racket function racketlist->mupllist that takes a Racket list (presumably of mupl
;; values but that will not affect your solution) and produces an analogous mupllist with the same
;; elements in the same order.

(define (racketlist->mupllist xs)
  (cond 
    [(null? xs) (aunit)]
    [(= (length xs) 1) (apair (car xs) (aunit))]
    [#t (apair (car xs) (racketlist->mupllist (cdr xs)))]
  )
)

;; Problem 2
;; Write a Racket function mupllist->racketlist that takes a mupllist (presumably of mupl
;; values but that will not affect your solution) and produces an analogous Racket list (of mupl
;; values) with the same elements in the same order.

(define (mupllist->racketlist mplist)
  (cond 
    [(aunit? mplist) null]
    [(aunit? (apair-e2 mplist)) (cons (apair-e1 mplist) null)]
    [#t (cons (apair-e1 mplist) (mupllist->racketlist (apair-e2 mplist)))]
  )
)

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  ;; if the env is empty
  ;; env = (cons (cons name1 value1) tail)
  ;; did not find name match, look into next level
  (
    cond 
    [(null? env) (error "unbound variable during evaluation" str)]
    [(equal? (car (car env)) str) (cdr (car env))]
    [#t (envlookup (cdr env) str)]
  )
)

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
        ;; if it's a variable, look up in the env and get the value
  (cond [(var? e) (envlookup env (var-string e))]
        ;; implementation of add operation
        [(add? e) 
         (let 
          (
             [v1 (eval-under-env (add-e1 e) env)]
             [v2 (eval-under-env (add-e2 e) env)]
          )
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; implementation of function, easy
        [(fun? e)
         (closure env e)]
        [(ifgreater? e)
         (let 
           (
             [v1 (eval-under-env (ifgreater-e1 e) env)]
             [v2 (eval-under-env (ifgreater-e2 e) env)]
           )
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1)  (int-num v2))
                   (let ([v3 (eval-under-env (ifgreater-e3 e) env)])
                     v3)
                   (let ([v4 (eval-under-env (ifgreater-e4 e) env)])
                     v4))
               (error "MUPL ifgreater e1 or e2 evaluates to non-number")))]

        ;; implementation of mlet
        ;; - firstly, evaluate the e to v1
        ;; - add to env with (cons (var v1))
        ;; - evaluate through  eval-under-env with new extended env
        ;; (struct mlet (var e body) #:transparent) 
        ;; (mlet (var "h") (int 2) (add (int 3) (var "h") ) )
        [(mlet? e)
         (let 
           (
             [v1 (eval-under-env (mlet-e e) env)]
           )
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v1) env))
          )]

        ;; implement of apair call
        [(call? e)
        ;; - evaluate the function expression (or just closure) to let it return closure
        ;; - evaluate the argument expression to let it return argument value
         (let ([v1 (eval-under-env (call-funexp e) env)]  ; should be closure
               [v2 (eval-under-env (call-actual e) env)]) ; should be argument value
           (if (closure? v1)
               (let ([fun-name (fun-nameopt (closure-fun v1))]      ; fetch the function name as fun-name
                     [argument-name (fun-formal (closure-fun v1))]  ; fetch the argument name as argument-name
                     [function-body (fun-body (closure-fun v1))])
                     ;; this part is really important!
                     ;; append the func-name if possible for recursive purpose
                     ;; also append the argument variable
                 (eval-under-env function-body (append (if fun-name 
                                                           (list (cons fun-name v1))
                                                                 null)
                                                           (cons (cons argument-name v2) (closure-env v1)))))
               (error "not a clusure error")))]

        ;; implemetation of apair
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]

        ;; implementation of fst
        [(fst? e)
         (let ([v1 (eval-under-env (fst-e e) env)])
           (if (apair? v1)
               (apair-e1 v1)
               (error "not a pair")))]
        
        ;; implementation of snd
        [(snd? e)
         (let ([v1 (eval-under-env (snd-e e) env)])
           (if (apair? v1)
               (apair-e2 v1)
               (error "not a pair")))]

        ;; implementation of isaunit
        [(isaunit? e)
         (let ([v1 (eval-under-env (isaunit-e e) env)])
           (if (aunit? v1)
               (int 1)
               (int 0)))]

        ;; implementation of the rest trivial
        [(or (int? e) (closure? e) (aunit? e)) e]
        [#t (error (format "bad expression" e))]))


;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

;; why Racket list of Racket pair??
(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (caar lstlst) (cdar lstlst) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "val1" e1) (cons "val2" e2))
         (ifgreater (var "val1") (var "val2") e4
                    (ifgreater (var "val2") (var "val1") e4
                               e3))))
;; Problem 4
;; (fun "doubleInt" "x" (* (int 2) (var "x")) )
(define mupl-map
  (fun #f "fun-map"
       ; function body
       (fun "f2" "mulplst"
            (ifaunit (var "mulplst")
                     (aunit)
                     (apair (call (var "fun-map") (fst (var "mulplst"))) (call (var "f2") (snd (var "mulplst"))))))))
       

(define mupl-mapAddN 
  (mlet "map" mupl-map
        ; function body
        (fun #f "int-i"
             (fun #f "mulplst"
                  (call (call (var "map") (fun #f "x" (add (var "x") (var "int-i")))) (var "mulplst"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
