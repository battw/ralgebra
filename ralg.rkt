#lang racket


(define (com expr)
  (match expr
    [`(+ ,a ,b) (list '+ b a)]
    [`(* ,a ,b) (list '* b a)]
    [_ (list 'does-not-commute expr)]))

(define (dis expr)
    (match expr
      [`(* ,a (+ ,b ,c)) (list '+ (list '* a b) (list '* a c))]
      [_ (list 'does-not-distribute expr)]))

(define (fac expr)
  (match expr
    [`(+ (* ,a ,b) (* ,a ,c)) (list '* a (list '+ b c))]
    [_ (list 'does-not-factor expr)]))

;; recurse and reconstruct (template for other functions)
(define (f expr)
  (if (not (list? expr))
      expr
      (match expr [`(,op ,l ,r) (list op (f l) (f r))])))
  
(define (app rule i expr)
  (cond
    [(not (list? expr)) expr]
    [(= i 1) (rule expr)]
    [else
     (match expr [`(,op ,l ,r) (list op
                                     (app rule (- i 1)  l)
                                     (app rule (- (- i 1) (size l)) r))])]))

(define (size expr)
  (if (not (list? expr))
      0
      (match expr
        [`(+ ,l ,r) (+ 1 (size l) (size r))]
        [`(* ,l ,r) (+ 1 (size l) (size r))])))
