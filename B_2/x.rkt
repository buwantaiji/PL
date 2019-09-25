#lang racket
(provide (all-defined-out))

(struct int (number) #:transparent) ; #number should be a racket integer.
(struct add (e1 e2) #:transparent)
(struct subtract (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)
(struct greater (e1 e2) #:transparent)
(struct smaller (e1 e2) #:transparent)
(struct equalto (e1 e2) #:transparent)

(struct bool (value) #:transparent) ; #value should be a racket boolean value(#t or #f).
(struct if-then-else (e1 e2 e3) #:transparent)

(struct xcons (e1 e2) #:transparent)
(struct xcar (e) #:transparent)
(struct xcdr (e) #:transparent)
(struct xnull () #:transparent)
(struct isxnull (e) #:transparent)


(struct def (var e) #:transparent) ; #var should be a racket string.
(struct var (name) #:transparent) ; #name should be a racket string.
(define (envlookup name env)
        (cond [(null? env) (error "Unbound variable during evaluation" name)]
              [(equal? name (car (car env))) (cdr (car env))]
              [#t (envlookup name (cdr env))]))
(struct fdef (var arg body) #:transparent) ; #var and #arg should be racket strings.
(struct xlambda (arg body) #:transparent) ; #arg should be a racket string.
(struct call (fun arge) #:transparent)
(struct closure (code env) #:transparent)

(struct xlet (var e body) #:transparent) ; #var should be (1) a racket string, in which case #e must be a X-expression.
                                         ;                (2) #f, in which case #e must be a X-fdef construct.

(define (eval-under-env e env)
        (cond [(int? e) e]
              [(add? e) (let ([v1 (eval-under-env (add-e1 e) env)]
                              [v2 (eval-under-env (add-e2 e) env)])
                             (if (and (int? v1) (int? v2))
                                 (int (+ (int-number v1) (int-number v2)))
                                 (error "X add applied to non-number.")))]
              [(subtract? e) (let ([v1 (eval-under-env (subtract-e1 e) env)]
                                   [v2 (eval-under-env (subtract-e2 e) env)])
                                  (if (and (int? v1) (int? v2))
                                      (int (- (int-number v1) (int-number v2)))
                                      (error "X subtract applied to non-number.")))]
              [(multiply? e) (let ([v1 (eval-under-env (multiply-e1 e) env)]
                                   [v2 (eval-under-env (multiply-e2 e) env)])
                                  (if (and (int? v1) (int? v2))
                                      (int (* (int-number v1) (int-number v2)))
                                      (error "X multiply applied to non-number.")))]
              [(greater? e) (let ([v1 (eval-under-env (greater-e1 e) env)]
                                  [v2 (eval-under-env (greater-e2 e) env)])
                                 (if (and (int? v1) (int? v2))
                                     (bool (> (int-number v1) (int-number v2)))
                                     (error "X greater applied to non-number.")))]
              [(smaller? e) (let ([v1 (eval-under-env (smaller-e1 e) env)]
                                  [v2 (eval-under-env (smaller-e2 e) env)])
                                 (if (and (int? v1) (int? v2))
                                     (bool (< (int-number v1) (int-number v2)))
                                     (error "X smaller applied to non-number.")))]
              [(equalto? e) (let ([v1 (eval-under-env (equalto-e1 e) env)]
                                  [v2 (eval-under-env (equalto-e2 e) env)])
                                 (if (and (int? v1) (int? v2))
                                     (bool (= (int-number v1) (int-number v2)))
                                     (error "X equalto applied to non-number.")))]
              [(bool? e) e]
              [(if-then-else? e) (let ([v1 (eval-under-env (if-then-else-e1 e) env)])
                                      (if (bool? v1)
                                          (if (bool-value v1)
                                              (eval-under-env (if-then-else-e2 e) env)
                                              (eval-under-env (if-then-else-e3 e) env))
                                          (eval-under-env (if-then-else-e2 e) env)))]
              [(xcons? e) (let ([v1 (eval-under-env (xcons-e1 e) env)]
                                [v2 (eval-under-env (xcons-e2 e) env)])
                               (xcons v1 v2))]
              [(xcar? e) (let ([pr (eval-under-env (xcar-e e) env)])
                              (if (xcons? pr)
                                  (xcons-e1 pr)
                                  (error "X xcar applied to non-pair.")))]
              [(xcdr? e) (let ([pr (eval-under-env (xcdr-e e) env)])
                              (if (xcons? pr)
                                  (xcons-e2 pr)
                                  (error "X xcdr applied to non-pair.")))]
              [(xnull? e) e]
              [(isxnull? e) (let ([v (eval-under-env (isxnull-e e) env)])
                                 (bool (xnull? v)))]
              [(def? e) (let* ([v (eval-under-env (def-e e) env)]
                               [extended-env (cons (cons (def-var e) v) env)])
                              extended-env)]
              [(var? e) (envlookup (var-name e) env)]
              [(fdef? e) (let ([extended-env (cons (cons (fdef-var e) (closure e env)) env)])
                              extended-env)]
              [(xlambda? e) (closure (fdef #f (xlambda-arg e) (xlambda-body e)) env)]
              [(call? e) (let ([cl (eval-under-env (call-fun e) env)])
                              (if (closure? cl)
                                  (let* ([argv (eval-under-env (call-arge e) env)]
                                         [funname (fdef-var (closure-code cl))]
                                         [funargname (fdef-arg (closure-code cl))]
                                         [funbody (fdef-body (closure-code cl))]
                                         [extended-env (if funname
                                                           (cons (cons funname cl) (cons (cons funargname argv) (closure-env cl)))
                                                           (cons (cons funargname argv) (closure-env cl)))])
                                        (eval-under-env funbody extended-env))
                                  (error "X call applied to non-function.")))]
              [(xlet? e) (cond [(xlet-var e) (let* ([v (eval-under-env (xlet-e e) env)]
                                                    [extended-env (cons (cons (xlet-var e) v) env)])
                                                   (eval-under-env (xlet-body e) extended-env))]
                               [(fdef? (xlet-e e)) (eval-under-env (xlet-body e) (eval-under-env (xlet-e e) env))]
                               [#t (error "When the #var part of X xlet is #f(not a string), the #e part should be a fdef binding.")]
                             )]
              [#t (error (format "bad MUPL expression: ~v" e))]))
(define (list->xlist xs)
        (if (null? xs)
            (xnull)
            (xcons (car xs) (list->xlist (cdr xs)))))
(define (xlist->list xxs)
        (if (xnull? xxs)
            null
            (cons (xcons-e1 xxs) (xlist->list (xcons-e2 xxs)))))

(define (eval-es es)
        (letrec ([eval-es-under-env (lambda (es env)
                                            (if (null? (cdr es))
                                                (eval-under-env (car es) env)
                                                (eval-es-under-env (cdr es) (eval-under-env (car es) env))))])
                (eval-es-under-env es null)))


(define testprogram (list
    (fdef "factorial" "n" (if-then-else (equalto (var "n") (int 0))
                                        (int 1)
                                        (multiply (var "n") (call (var "factorial") (subtract (var "n") (int 1))))))
    (fdef "sum" "n" (if-then-else (equalto (var "n") (int 0))
                                        (int 0)
                                        (add (var "n") (call (var "sum") (subtract (var "n") (int 1))))))
    (fdef "sum-tail" "n" (xlet #f (fdef "f" "n" (xlambda "acc"
                                                         (if-then-else (equalto (var "n") (int 0))
                                                                       (var "acc")
                                                                       (call (call (var "f") (subtract (var "n") (int 1))) (add (var "acc") (var "n"))))))
                                  (call (call (var "f") (var "n")) (int 0))))
    (fdef "length" "xxs" (if-then-else (isxnull (var "xxs"))
                                       (int 0)
                                       (add (int 1) (call (var "length") (xcdr (var "xxs"))))))
    (fdef "sumlist" "xxs" (if-then-else (isxnull (var "xxs"))
                                        (int 0)
                                        (add (xcar (var "xxs")) (call (var "sumlist") (xcdr (var "xxs"))))))
    (fdef "append" "xxs" (xlambda "xys"
                                  (if-then-else (isxnull (var "xxs"))
                                                (var "xys")
                                                (xcons (xcar (var "xxs"))
                                                       (call (call (var "append") (xcdr (var "xxs"))) (var "xys"))))))
    (fdef "map" "f" (xlambda "xxs"
                             (if-then-else (isxnull (var "xxs"))
                                           (xnull)
                                           (xcons (call (var "f") (xcar (var "xxs")))
                                                  (call (call (var "map") (var "f")) (xcdr (var "xxs")))))))
    ;(call (var "sumlist") (list->xlist (list (int 3) (int 6) (int 2) (int 9) (int 15) (int 7) (int 20))))
    (def "list1" (list->xlist (list (int 1) (int 2) (int 3) (int 4) (int 5))))
    (def "list2" (list->xlist (list (int 11) (int 22) (int 33) (int 44))))
    ;(call (call (var "append") (var "list1")) (var "list2"))
    ;(call (var "sum-tail") (int 37))
    (call (call (var "map") (xlambda "x" (multiply (var "x") (var "x")))) (var "list1"))
))
