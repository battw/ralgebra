#lang racket

(define commutative-ops '(+ *))
(define left-distributive-ops '((* . +)))
(define right-distributive-ops '((* . +)))
(define associative-ops '(+ *))
(define left-identities '((+ . 0) (* . 1)))
(define right-identities '((+ . 0) (* . 1)))
;; lists of
;; (operation function-giving-the-inverse-of-its-input resulting-identity)
(define left-inverses '(
                        (+  (lambda (a) (list '- a))  0)
                        (*  (lambda (a) (list '/ 1 a))  1)
                        ))
(define right-inverses '(
                         (+  (lambda (a) (list '- a))  0)
                         (*  (lambda (a) (list '/ 1 a))  1)
                         ))

(define (commute expr)
  (match expr
    [`(,op ,a ,b)
     #:when (member op commutative-ops)
     (list op b a)]
    [_ (list 'does-not-commute expr)]
    ))

(define (left-distribute expr)
    (match expr
      [`(,op1 ,a (,op2 ,b ,c))
       #:when (member (cons op1 op2) left-distributive-ops)
       (list op2 (list op1 a b) (list op1 a c))]
      [`(,op1 (,op2 ,a ,b) (,op2 ,a ,c))
       #:when (member (cons op2 op1) left-distributive-ops)
       (list op2 a (list op1 b c))]
      [_ (list 'does-not-distribute expr)]
      ))


(define (right-distribute expr)
    (match expr
      [`(,op1 (,op2 ,a ,b) ,c)
       #:when (member (cons op1 op2) right-distributive-ops)
       (list op2 (list op1 a c) (list op1 b c))]
      [`(,op2 (,op1 ,a ,c) (,op1 ,b ,c))
       #:when (member (cons op1 op2) right-distributive-ops)
       (list op1 (list op2 a b) c)]
      [_ (list 'does-not-distribute expr)]
      ))

(define (associate expr)
  (match expr
    [`(,op ,a (,op ,b ,c))
     #:when (member op associative-ops)
     (list op (list op a b) c)]
    [`(,op (,op ,a ,b) ,c)
     #:when (member op associative-ops)
     (list op a (list op b c))]
    [_ (list 'does-not-associate expr)]
    ))

(define (left-identity expr)
  (match expr
    [`(,op, id ,sub)
     #:when (member (cons id op) left-identities) sub]
    [_ (list 'isnt-left-identity expr)]
    ))

(define (right-identity expr)
  (match expr
    [`(,op ,sub, id)
     #:when (member (cons op id) right-identities) sub]
    [_ (list 'isnt-right-identity expr)]
    ))

(define (left-inverse expr)
  (match expr
    [`(,op ,inv ,a)
     #:when (equal? ((eval (cadr (assoc op left-inverses))) a)
                    inv)
     (caddr (assoc op left-inverses))]
    [_ (list 'isnt-left-inverse expr)]
    ))
  
(define (right-inverse expr)
  (match expr
    [`(,op ,a ,inv)
     #:when (equal? ((eval (cadr (assoc op right-inverses))) a)
                    inv)
     (caddr (assoc op right-inverses))]
    [_ (list 'isnt-right-inverse expr)]
    ))

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
                                     (apply rule r (- (- i 1) (size l)))
                                     )])]))

(define (size expr)
  ;; number of operators
  (if (not (list? expr))
      0
      (match expr
        [`(,op ,l ,r) (+ 1 (size l) (size r))])))

(define (com expr i)
  (apply commute expr i))

(define (ldis expr i)
  (apply left-distribute expr i))

(define (rdis expr i)
  (apply right-distribute expr i))

(define (ass expr i)
  (apply associate expr i))

(define (lid expr i)
  (apply left-identity expr i))

(define (rid expr i)
  (apply right-identity expr i))

(define (linv expr i)
  (apply left-inverse expr i))

(define (rinv expr i)
  (apply right-inverse expr i))

