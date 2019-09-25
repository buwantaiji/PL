#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
; 1 ;
(define (sequence low high stride)
        (if (> low high)
            null
            (cons low (sequence (+ low stride) high stride))))
; 2 ;
(define (string-append-map strs suffix)
        (map (lambda (str) (string-append str suffix)) strs))
; 3 ;
(define (list-nth-mod xs n)
        (cond [(< n 0) (error "list-nth-mod: negative number")]
              [(null? xs) (error "list-nth-mod: empty list")]
              [#t (car (list-tail xs (remainder n (length xs))))]))
; 4 ;
(define (stream-for-n-steps stream n)
        (if (= n 0)
            null
            (let ([pr (stream)])
                 (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))
; 5 ;
(define (funny-number-stream)
        (letrec ([f (lambda (n)
                            (cons (if (= (remainder n 5) 0) (- n) n) (lambda () (f (+ n 1)))))])
                (f 1)))
; 6 ;
(define (dan-then-dog)
        (letrec ([f (lambda (str)
                            (cons str (lambda () (f (if (string=? str "dan.jpg") "dog.jpg" "dan.jpg")))))])
                (f "dan.jpg")))
; 7 ;
(define (stream-add-zero stream)
        (letrec ([f (lambda (s)
                            (let ([pr (s)])
                                 (cons (cons 0 (car pr)) (lambda () (f (cdr pr))))))])
                (lambda () (f stream))))
; 8 ;
(define (cycle-lists xs ys)
        (letrec ([f (lambda (n)
                            (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))))])
                (lambda () (f 0))))
; 9 ;
(define (vector-assoc val vec)
        (letrec ([iter (lambda (n)
                               (if (= n (vector-length vec))
                                   #f
                                   (let ([pr (vector-ref vec n)])
                                        (cond [(not (pair? pr)) (iter (+ n 1))]
                                              [(equal? (car pr) val) pr]
                                              [#t (iter (+ n 1))]))))])
                (iter 0)))
; 10 ;
(define (cached-assoc xs n)
        (letrec ([cache (make-vector n #f)]
                 [slot 0]
                 [f (lambda (val)
                            (let ([pr (vector-assoc val cache)])
                                 (if pr
                                     ; For test, substitute the line below with this line: (begin (display "Using cache!") (cdr pr))
                                     (cdr pr)
                                     (let ([result (assoc val xs)])
                                          (if result
                                              (begin (vector-set! cache slot (cons val result))
                                                     (set! slot (remainder (+ slot 1) n))
                                                     result)
                                              result)))))])
                f))
(define (testlist n)
        (letrec ([f (lambda (i acc)
                            (if (> i n) acc (f (+ i 1) (cons (cons i (+ i 1)) acc))))])
                (f 1 null)))
; Testing:
; (define xs (testlist 60000000))
; (define new-assoc (cached-assoc xs 10))
; (new-assoc 1)
; (new-assoc 1)
; Note the second call of new-assoc will use the cache.
