;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; 1. a. Convert a Racket list to a MUPL list.
(define (racketlist->mupllist ls)
  (if (null? ls)
      (aunit)
      (apair (car ls) (racketlist->mupllist (cdr ls)))))

;; 1. b. Convert a MUPL list to a Racket list.
(define (mupllist->racketlist ls)
  (if (aunit? ls)
      null
      (cons (apair-e1 ls) (mupllist->racketlist (apair-e2 ls)))))



;; 2. MUPL interpreter.

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e)                    ; var
         (envlookup env (var-string e))]
        
        [(add? e)                    ; add
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]

        [(ifgreater? e)              ; ifgreater
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]

        [(apair? e)                  ; apair
         (apair (eval-under-env (apair-e1 e) env)
                (eval-under-env (apair-e2 e) env))]

        [(fst? e)                    ; fst
         (let ([first (eval-under-env (fst-e e) env)])
           (if (apair? first)
               (eval-under-env (apair-e1 first) env)
               (error "MUPL fst applied to non-pair")))]

        [(snd? e)                    ; snd
         (let ([second (eval-under-env (snd-e e) env)])
           (if (apair? second)
               (eval-under-env (apair-e2 second) env)
               (error "MUPL snd applied to non-pair")))]

        [(isaunit? e)                ; isaunit
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        
        [(or (aunit? e)
             (int? e)
             (closure? e)) e]        ; aunit, int, closure

        [(mlet? e)                   ; mlet
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e)
                           (cons (cons (mlet-var e) v) env)))]

        ;fun  (nameopt formal body)
        ;closure (env fun)
        [(fun? e)                    ; fun
         (closure env e)]

        [(call? e)                   ; call
         (let ([c   (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (closure? c)
               (let* ([fun-env (closure-env c)] ;closure environment
                      [f       (closure-fun c)] ;function itself
                      [f-name  (fun-nameopt f)] ;function name, optional
                      [f-form  (fun-formal  f)] ;function argument name
                      [f-body  (fun-body    f)] ;function body
                      [e-name  (if f-name       ;env extended with function name
                                   (cons (cons f-name c) fun-env)
                                   fun-env)]
                      [e-arg   (cons (cons f-form arg) e-name)]) ;env extended with calling argument
                 (eval-under-env f-body e-arg))
               (error "MUPL call applied to non-closure")))]

        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))


;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* ls e2)
  (if (null? ls)
      e2
      (mlet (car (car ls)) (cdr (car ls)) (mlet* (cdr ls) e2))))
    

(define (ifeq e1 e2 e3 e4) (mlet* (list (cons "_x" e1) (cons "_y" e2))
                                  (ifgreater (var "_x") (var "_y")
                                             e4
                                             (ifgreater (var "_y") (var "_x")
                                                        e4
                                                        e3))))

;; Problem 4

(define mupl-map (fun #f "f"
                      (fun "map" "ls"
                           (ifaunit (var "ls")
                                    (aunit)
                                    (apair (call (var "f") (fst (var "ls")))
                                           (call (var "map") (snd (var "ls"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (fun #f "ls"
                  (call (call mupl-map (fun #f "x" (add (var "x") (var "i"))))
                                  (var "ls"))))))

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
