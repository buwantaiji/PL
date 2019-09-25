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

;; Problem 1
; a ;
(define (racketlist->mupllist racketxs)
        (if (null? racketxs)
            (aunit)
            (apair (car racketxs) (racketlist->mupllist (cdr racketxs)))))
; b ;
(define (mupllist->racketlist muplxs)
        (if (aunit? muplxs)
            null
            (cons (apair-e1 muplxs) (mupllist->racketlist (apair-e2 muplxs)))))

;; Problem 2

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
  (cond [(int? e) e]
        [(var? e) (envlookup env (var-string e))]
        [(add? e) (let ([v1 (eval-under-env (add-e1 e) env)]
                        [v2 (eval-under-env (add-e2 e) env)])
                       (if (and (int? v1) (int? v2))
                           (int (+ (int-num v1) 
                                   (int-num v2)))
                           (error "MUPL addition applied to non-number")))]
        [(ifgreater? e) (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
                              [v2 (eval-under-env (ifgreater-e2 e) env)])
                             (if (and (int? v1) (int? v2))
                                 (if (> (int-num v1) (int-num v2))
                                     (eval-under-env (ifgreater-e3 e) env)
                                     (eval-under-env (ifgreater-e4 e) env))
                                 (error "MUPL ifgreater compared non-number")))]
        [(apair? e) (let ([v1 (eval-under-env (apair-e1 e) env)]
                          [v2 (eval-under-env (apair-e2 e) env)])
                         (apair v1 v2))]
        [(fst? e) (let ([v (eval-under-env (fst-e e) env)])
                       (if (apair? v)
                           (apair-e1 v)
                           (error "MUPL fst expected a pair")))]
        [(snd? e) (let ([v (eval-under-env (snd-e e) env)])
                       (if (apair? v)
                           (apair-e2 v)
                           (error "MUPL snd expected a pair")))]
        [(aunit? e) e]
        [(isaunit? e) (let ([v (eval-under-env (isaunit-e e) env)])
                           (if (aunit? v)
                               (int 1)
                               (int 0)))]
        [(fun? e) (closure env e)]
        [(call? e) (let ([funv (eval-under-env (call-funexp e) env)]
                         [argv (eval-under-env (call-actual e) env)])
                        (if (not (closure? funv))
                            (error "MUPL call applied to non-function")
                            (let* ([funenv (closure-env funv)]
                                   [funname (fun-nameopt (closure-fun funv))]
                                   [funargname (fun-formal (closure-fun funv))]
                                   [funbody (fun-body (closure-fun funv))]
                                   [extended-funenv (if funname
                                                        (cons (cons funname funv) (cons (cons funargname argv) funenv))
                                                        (cons (cons funargname argv) funenv))])
                                  (eval-under-env funbody extended-funenv))))]
        [(mlet? e) (let* ([varname (mlet-var e)]
                          [varvalue (eval-under-env (mlet-e e) env)]
                          [letbody (mlet-body e)]
                          [extended-env (cons (cons varname varvalue) env)])
                         (eval-under-env letbody extended-env))]
        ; This branch is useful only for testing. Real MUPL program processing will not get here.
        [(closure? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

; Two simple test programs for recursion and high-order function.
(define testprogram1 (call (fun "add-factorial" "n" (ifgreater (var "n")
                                                              (int 0)
                                                              (add (var "n") (call (var "add-factorial") (add (var "n") (int -1))))
                                                              (int 0)))
                          (int 36)))
(define testprogram2 (call (fun "testfun" "f" (call (var "f") (int 1000)))
                           (fun #f "x" (add (var "x") (int 1)))))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3
; a ;
(define (ifaunit e1 e2 e3)
        (ifgreater (isaunit e1) (int 0) e2 e3))
; b ;
; Example usage of the "macro" mlet*:
; (mlet* (list (cons "x" (int 3)) (cons "y" (int 4))) (add (var "x") (var "y")))   ===>   (eval-exp ...) will get (int 7).
; (mlet* (list (cons "x" (int 3)) (cons "y" (add (var "x") (int 100)))) (add (var "x") (var "y")))   ===>   (eval-exp ...) will get (int 106).
(define (mlet* lstlst e2)
        (if (null? lstlst)
            e2
            (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))
; c ;
; Example usage of the "macro" ifeq:
; (ifeq (add (int 4) (int 3)) (int 7) (int 333) (int 444))   ===>   (eval-exp ...) will get (int 333).
; (ifeq (add (int 4) (int 5)) (int 7) (int 333) (int 444))   ===>   (eval-exp ...) will get (int 444).
(define (ifeq e1 e2 e3 e4)
        (mlet* (list (cons "v1" e1) (cons "v2" e2))
               (ifgreater (var "v1")
                          (var "v2")
                          e4
                          (ifgreater (var "v2")
                                     (var "v1")
                                     e4
                                     e3))))

;; Problem 4
; a ;
; Example usage:
; (define test (call (call mupl-map (fun #f "x" (add (var "x") (int 100))))
;                    (racketlist->mupllist (list (int 1) (int 2) (int 3) (int 4) (int 5)))))
; ===> (mupllist->racketlist (eval-exp test)) will get (list (int 101) (int 102) (int 103) (int 104) (int 105)).
(define mupl-map
        (fun "mupl-map" "f" (fun "func" "muplxs" (ifaunit (var "muplxs")
                                                          (aunit)
                                                          (apair (call (var "f") (fst (var "muplxs")))
                                                                 (call (var "func") (snd (var "muplxs"))))))))
; b ;
; (define test (call (call mupl-mapAddN (int 100))
;                    (racketlist->mupllist (list (int 1) (int 2) (int 3) (int 4) (int 5)))))
; ===> (mupllist->racketlist (eval-exp test)) will get (list (int 101) (int 102) (int 103) (int 104) (int 105)).
(define mupl-mapAddN 
  (mlet "map" mupl-map (fun #f "n" (call (var "map") (fun #f "x" (add (var "x") (var "n")))))))

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
