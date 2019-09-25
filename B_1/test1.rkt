#lang racket
(provide (all-defined-out))

(define (sum xs)
        (if (null? xs)
            0
            (+ (car xs) (sum (cdr xs)))))
(define (my-append xs ys)
        (if (null? xs)
            ys
            (cons (car xs) (my-append (cdr xs) ys))))
(define (my-map f xs)
        (if (null? xs)
            null
            (cons (f (car xs)) (my-map f (cdr xs)))))

; eager evaluation(call-by-value)
(define (slow-add x y)
        (letrec ([sillyf (lambda (n result)
                                 (if (= n 0) result (sillyf (- n 1) result)))])
                (sillyf 3000000000 (+ x y))))
(define (silly-mult x y)
        (if (= x 0)
            0
            (+ y (silly-mult (- x 1) y))))
; delayed evaluation, using thunk
(define (silly-mult-th x thunk)
        (if (= x 0)
            0
            (+ (thunk) (silly-mult-th (- x 1) thunk))))
; an implementation of lazy evaluation(call-by-need), using delay and force
(define (my-delay thunk)
        (mcons #f thunk))
(define (my-force promise)
        (if (mcar promise)
            (mcdr promise)
            (begin (set-mcar! promise #t)
                   (set-mcdr! promise ((mcdr promise)))
                   (mcdr promise))))
(define (silly-mult-lazy x promise)
        (if (= x 0)
            0
            (+ (my-force promise) (silly-mult-lazy (- x 1) promise))))

; the generating and using of streams
(define (nat)
        (letrec ([f (lambda (n) (cons n (lambda () (f (+ n 1)))))])
                (f 1)))
(define (powers-of-two)
        (letrec ([f (lambda (n) (cons n (lambda () (f (* n 2)))))])
                (f 2)))
