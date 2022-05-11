#lang racket

(provide (all-defined-out))

;;;; Programming Languages — Homework 5

;;; Provided definitions of structures for MUPL programs:

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

;; Provided: a closure is not in "source" programs but /is/ a MUPL value;
;; it is what functions evaluate to:

(struct closure (env fun) #:transparent)

;;; Problem 1

;; 1a.

(define (racketlist->mupllist lst)
  (foldr (λ (x y) (apair x y)) (aunit) lst))

;; 1b.

(define (mupllist->racketlist mupl-lst)
  (define (iter backward-ans mupl-lst)
    (if (aunit? (apair-e2 mupl-lst))
        (cons (apair-e1 mupl-lst) backward-ans)
        (iter (cons (apair-e1 mupl-lst) backward-ans)
              (apair-e2 mupl-lst))))
  (if (aunit? mupl-lst) null (reverse (iter null mupl-lst))))

;;; Problem 2

;; Provided code to lookup a variable in an environment:

(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Function to eval a MUPL expression under a given environment;
;; The first two cases (var and add) were provided:

(define (eval-under-env e env)
  (cond
    [(aunit? e) e]
    [(int? e) e]
    [(closure? e) e]
    [(fun? e) (closure env e)]
    [(var? e) (envlookup env (var-string e))]
    [(add? e)
     (let ([v1 (eval-under-env (add-e1 e) env)]
           [v2 (eval-under-env (add-e2 e) env)])
       (if (and (int? v1)
                (int? v2))
           (int (+ (int-num v1)
                   (int-num v2)))
           (error "MUPL addition applied to non-number")))]
    [(ifgreater? e)
     (define v1 (eval-under-env (ifgreater-e1 e) env))
     (define v2 (eval-under-env (ifgreater-e2 e) env))
     (cond
       [(and (int? v1) (int? v2))
        (if (> (int-num v1) (int-num v2))
            (eval-under-env (ifgreater-e3 e) env)
            (eval-under-env (ifgreater-e4 e) env))]
       [else (error "MUPL ifgreater applied to non-numbers")])]
    [(mlet? e)
     (define var (cons (mlet-var e) (eval-under-env (mlet-e e) env)))
     (eval-under-env (mlet-body e) (cons var env))]
    [(call? e)
     (define funexp (eval-under-env (call-funexp e) env))
     (cond
       [(closure? funexp)
        (define nameopt (cons (fun-nameopt (closure-fun funexp)) funexp))
        (define argument (cons (fun-formal (closure-fun funexp))
                               (eval-under-env (call-actual e) env)))
        (eval-under-env (fun-body (closure-fun funexp))
                        (if (car nameopt)
                            (list* nameopt argument (closure-env funexp))
                            (cons argument (closure-env funexp))))]
       [else (error "MUPL call applied to non-closure:" funexp)])]
    [(apair? e)
     (apair (eval-under-env (apair-e1 e) env)
            (eval-under-env (apair-e2 e) env))]
    [(fst? e)
     (define maybe-pair (eval-under-env (fst-e e) env))
     (if (apair? maybe-pair)
         (apair-e1 maybe-pair)
         (error "MUPL fst applied to non-pair:" maybe-pair))]
    [(snd? e)
     (define maybe-pair (eval-under-env (snd-e e) env))
     (if (apair? maybe-pair)
         (apair-e2 maybe-pair)
         (error "MUPL snd applied to non-pair:" maybe-pair))]
    [(isaunit? e)
     (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
    [else (error (format "Bad MUPL expression: ~v" e))]))

;; Provided wrapper for the function above:

(define (eval-exp e)
  (eval-under-env e null))

;;; Problem 3

;; 3a.

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

;; 3b.

(define (mlet* lstlst e2)
  (foldr (λ (x y) (mlet (car x) (cdr x) y)) e2 lstlst))

;; 3c.

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y")
                    e4
                    (ifgreater (var "_y") (var "_x") e4 e3))))

;;; Problem 4

(define mupl-map
  (fun #f "fn"
       (fun "iter" "lst"
            (ifaunit (var "lst")
                     (aunit)
                     (apair (call (var "fn") (fst (var "lst")))
                            (call (var "iter") (snd (var "lst"))))))))

(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun #f "i"
             (fun #f "lst"
                  (call (call (var "map")
                              (fun #f "x" (add (var "x") (var "i"))))
                        (var "lst"))))))

;;; Challenge Problem

;; Provided struct: a recursive (?) 1-argument function:

(struct fun-challenge (nameopt formal body freevars) #:transparent)

(define (compute-free-vars e)
  (define (iter exp vars)
    (match exp
      [(or (aunit) (int _) (closure _ _) (fun-challenge . _))
       (cons exp vars)]
      [(struct var (str)) (cons exp (set-add vars str))]
      [(add e1 e2)
       (define v1 (iter e1 vars))
       (define v2 (iter e2 vars))
       (cons (add (car v1) (car v2))
             (set-union (cdr v1) (cdr v2)))]
      [(fun nameopt formal body)
       (define v1 (iter body vars))
       (define freevars
         (set-remove (if nameopt (set-remove (cdr v1) nameopt) (cdr v1))
                     formal))
       (cons (fun-challenge nameopt formal (car v1) freevars) freevars)]
      [(ifgreater e1 e2 e3 e4)
       (define v1 (iter e1 vars))
       (define v2 (iter e2 vars))
       (define v3 (iter e3 vars))
       (define v4 (iter e4 vars))
       (cons (ifgreater (car v1) (car v2) (car v3) (car v4))
             (set-union (cdr v1) (cdr v2) (cdr v3) (cdr v4)))]
      [(call funexp actual)
       (define v1 (iter funexp vars))
       (define v2 (iter actual vars))
       (cons (call (car v1) (car v2))
             (set-union (cdr v1) (cdr v2)))]
      [(mlet var e body)
       (define v1 (iter e vars))
       (define v2 (iter body vars))
       (cons (mlet var (car v1) (car v2))
             (set-remove (set-union (cdr v1) (cdr v2)) var))]
      [(apair e1 e2)
       (define v1 (iter e1 vars))
       (define v2 (iter e2 vars))
       (cons (apair (car v1) (car v2))
             (set-union (cdr v1) (cdr v2)))]
      [(fst e)
       (define v1 (iter e vars))
       (cons (fst (car v1)) (cdr v1))]
      [(snd e)
       (define v1 (iter e vars))
       (cons (snd (car v1)) (cdr v1))]
      [(isaunit e)
       (define v1 (iter e vars))
       (cons (isaunit (car v1)) (cdr v1))]))
  (car (iter e (set))))

(define (eval-under-env-c e env)
  (match e
    [(or (aunit) (int _) (closure _ _)) e]
    [(struct var (string)) (envlookup env string)]
    [(fun _ _ _) (eval-under-env-c (compute-free-vars e) env)]
    [(fun-challenge nameopt formal body freevars)
     (closure (set-map freevars (λ (str) (cons str (envlookup env str))))
              (fun nameopt formal body))]
    [(add e1 e2)
     (define v1 (eval-under-env-c e1 env))
     (define v2 (eval-under-env-c e2 env))
     (if (and (int? v1) (int? v2))
         (int (+ (int-num v1) (int-num v2)))
         (error "MUPL addition applied to non-numbers"))]
    [(ifgreater e1 e2 e3 e4)
     (define v1 (eval-under-env-c e1 env))
     (define v2 (eval-under-env-c e2 env))
     (cond [(and (int? v1) (int? v2))
            (if (> (int-num v1) (int-num v2))
                (eval-under-env-c e3 env)
                (eval-under-env-c e4 env))]
           [else (error "MUPL ifgreater applied to non-numbers.")])]
    [(mlet s e1 e2)
     (define local-binding (cons s (eval-under-env-c e1 env)))
     (eval-under-env-c e2 (cons local-binding env))]
    [(call funexp actual)
     (define function (eval-under-env-c funexp env))
     (define argument (eval-under-env-c actual env))
     (match function
       [(closure local-env (fun nameopt formal body))
        (eval-under-env-c body (if nameopt
                                   (list* (cons nameopt function)
                                          (cons formal argument)
                                          local-env)
                                   (cons (cons formal argument)
                                         local-env)))]
       [_ (error "MUPL call applied to non-closure:" function)])]
    [(apair e1 e2)
     (apair (eval-under-env-c e1 env)
            (eval-under-env-c e2 env))]
    [(fst e)
     (define maybe-pair (eval-under-env-c e env))
     (if (apair? maybe-pair)
         (apair-e1 maybe-pair)
         (error "MUPL fst applied to non-pair:" maybe-pair))]
    [(snd e)
     (define maybe-pair (eval-under-env-c e env))
     (if (apair? maybe-pair)
         (apair-e2 maybe-pair)
         (error "MUPL snd applied to non-pair:" maybe-pair))]
    [(isaunit e)
     (define maybe-unit (eval-under-env-c e env))
     (if (aunit? maybe-unit) (int 1) (int 0))]
    [_ (error "Bad MUPL expression:" e)]))

;; Provided wrapper:

(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
