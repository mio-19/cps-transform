#lang racket
(define (T exp k)
  (match exp
    [`(if ,b ,x ,y) (T b (λ (b1) `(if ,b1 ,(T x k) ,(T y k))))]
    [`(λ (,@xs) ,body) (define k1 (gensym)) (k `(λ (,@xs ,k1) ,(T body (λ (b1) `(,k1 ,b1)))))]
    [`(,(and f (or 'cons 'car 'cdr 'null?)) ,@xs) (T* xs (λ (xs1) (k `(,f ,@xs1))))]
    [`(,f ,@xs) (define k1 (gensym)) (T f (λ (f1) (T* xs (λ (xs1) `(,f1 ,@xs1 (λ (,k1) ,(k k1)))))))]
    [_ (k exp)]))
(define (T* xs k) (if (null? xs) (k '()) (T (car xs) (λ (v1) (T* (cdr xs) (λ (vs) (k (cons v1 vs))))))))
(define (cps-transform exp) (T exp (λ (x) x)))
