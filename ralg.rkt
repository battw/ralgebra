#lang racket

(define commutative-ops '(+ *))
(define distributive-ops '((* . +)))
(define associative-ops '(+ *))
(define left-identities '((+ . 0) (* . 1)))

(define (commute expr)
  (match expr
    [`(,op ,a ,b)
     #:when (member op commutative-ops)
     (list op b a)]
    [_ (list 'does-not-commute expr)]))

(define (distribute expr)
    (match expr
      [`(,op1 ,a (,op2 ,b ,c))
       #:when (member (cons op1 op2) distributive-ops)
       (list op2 (list op1 a b) (list op1 a c))]
      [`(,op1 (,op2 ,a ,b) (,op2 ,a ,c))
       #:when (member (cons op2 op1) distributive-ops)
       (list op2 a (list op1 b c))]
      [_ (list 'does-not-distribute expr)]))


(define (associate expr)
  (match expr
    [`(,op ,a (,op ,b ,c))
     #:when (member op associative-ops)
     (list op (list op a b) c)]
    [`(,op (,op ,a ,b) ,c)
     #:when (member op associative-ops)
     (list op a (list op b c))]
    [_ (list 'does-not-associate expr)]))

(define (left-identity expr)
  (match expr
    [`(,op, id ,sub)
     #:when (member (cons op id) left-identities) sub]
    [_ (list 'isnt-left-identity expr)]))

  
;; recurse and reconstruct (template for other functions)
(define (f expr)
  (if (not (list? expr))
      expr
      (match expr [`(,op ,l ,r) (list op (f l) (f r))])))
  

(define (apply rule expr i)
  ;; apply the rule to the sub-expression at index i
  (cond
    [(not (list? expr)) expr]
    [(= i 1) (rule expr)]
    [else
     (match expr [`(,op ,l ,r) (list op
                                     (apply rule  l (- i 1))
                                     (apply rule r (- (- i 1) (size l))))])]))

(define (size expr)
  ;; number of operators
  (if (not (list? expr))
      0
      (match expr
        [`(,op ,l ,r) (+ 1 (size l) (size r))])))

(define (com expr i)
  (apply commute expr i))

(define (dis expr i)
  (apply distribute expr i))

(define (ass expr i)
  (apply associate expr i))

(define (lid expr i)
  (apply left-identity expr i))
