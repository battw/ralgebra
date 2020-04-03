#lang racket


(define (commute expr)
  (match expr
    [`(+ ,a ,b) (list '+ b a)]
    [`(* ,a ,b) (list '* b a)]
    [_ (list 'does-not-commute expr)]))

(define (distribute expr)
    (match expr
      [`(* ,a (+ ,b ,c)) (list '+ (list '* a b) (list '* a c))]
      [_ (list 'does-not-distribute expr)]))

(define (factor expr)
  (match expr
    [`(+ (* ,a ,b) (* ,a ,c)) (list '* a (list '+ b c))]
    [_ (list 'does-not-factor expr)]))

;; recurse and reconstruct (template for other functions)
(define (f expr)
  (if (not (list? expr))
      expr
      (match expr [`(,op ,l ,r) (list op (f l) (f r))])))
  
(define (apply rule expr i)
  (cond
    [(not (list? expr)) expr]
    [(= i 1) (rule expr)]
    [else
     (match expr [`(,op ,l ,r) (list op
                                     (apply rule  l (- i 1))
                                     (apply rule r (- (- i 1) (size l))))])]))

(define (size expr)
  (if (not (list? expr))
      0
      (match expr
        [`(+ ,l ,r) (+ 1 (size l) (size r))]
        [`(* ,l ,r) (+ 1 (size l) (size r))])))

(define (com expr i)
  (apply commute expr i))

(define (dis expr i)
  (apply distribute expr i))

(define (fac expr i)
  (apply factor expr i)) 
