#lang racket


(define binary-ops '(+ * - /))
(define unary-ops '(-))
(define ops (append binary-ops unary-ops))

(define commutative-ops '(+ *))
(define left-distributive-ops '((* . +)))
(define right-distributive-ops '((* . +) (/ . +)))
(define associative-ops '(+ *))
(define left-identities '((+ . 0) (* . 1)))
(define right-identities '((+ . 0) (* . 1) (/ . 1)))
;; lists of
;; (operation function-giving-the-inverse-of-its-input resulting-identity)
(define left-inverses (list
                        (list '+  (lambda (a) (list '- a))  0)
                        (list '*  (lambda (a) (list '/ 1 a))  1)
                        (list '/  (lambda (a) a)  1)
                        ))
(define right-inverses (list
                         (list '+  (lambda (a) (list '- a))  0)
                         (list '*  (lambda (a) (list '/ 1 a))  1)
                         (list '/  (lambda (a) a)  1)
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
    [`(,op ,id ,sub)
     #:when (member (cons op id) left-identities) sub]
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
     #:when (equal? ((cadr (assoc op left-inverses)) a)
                    inv)
     (caddr (assoc op left-inverses))]
    [_ (list 'isnt-left-inverse expr)]
    ))
  
(define (right-inverse expr)
  (match expr
    [`(,op ,a ,inv)
     #:when (equal? ((cadr (assoc op right-inverses)) a)
                    inv)
     (caddr (assoc op right-inverses))]
    [_ (list 'isnt-right-inverse expr)]
    ))

;; recurse and reconstruct (template for other functions)
(define (f expr)
  (if (not (list? expr))
      expr
      (match expr [`(,op ,a ,b) (list op (f a) (f b))])))
  

(define (employ rule expr i)
  ;; employ the rule to the sub-expression at index i
  (cond
    [(not (list? expr)) expr]
    [(= i 1) (rule expr)]
    [else
     (match expr
       [`(,op ,a ,b) (list op
                           (employ rule  a (- i 1))
                           (employ rule b (- (- i 1) (size a)))
                           )]
       [_ (list 'no-match-in-employ expr)]
       )]))


(define (size expr)
  (let ([f (lambda (acc a ex)
             (if (member a ops) (+ 1 acc) acc)
             )])
    (prefoldl f 0 expr)
    ))

(define (prefoldl f acc expr)
  ;; a left fold over an expression applying f in preorder
  (match expr
    [`(,op ,a ,b) (prefoldl f (prefoldl f (f acc op expr) a) b)]
    [`(,op ,a) (prefoldl f (f acc op expr) a)]
    [`,a (f acc a expr)]
    ))



(define (com expr i)
  (employ commute expr i))

(define (ldis expr i)
  (employ left-distribute expr i))

(define (rdis expr i)
  (employ right-distribute expr i))

(define (ass expr i)
  (employ associate expr i))

(define (lid expr i)
  (employ left-identity expr i))

(define (rid expr i)
  (employ right-identity expr i))

(define (linv expr i)
  (employ left-inverse expr i))

(define (rinv expr i)
  (employ right-inverse expr i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-tests)
      (map
       (lambda (test) (apply test '())) 
       (list
        (lambda () (equal? (com '(* x y) 1)
                      '(* y x)))
        (lambda () (equal? (ldis '(* x (+ y z)) 1)
                      '(+ (* x y) (* x z))))
        (lambda () (equal? (rdis '(* (+ 1 2) 3) 1)
                      '(+ (* 1 3) (* 2 3))))
        (lambda () (equal? (ass '(+ 1 (+ 2 3)) 1)
                      '(+ (+ 1 2) 3)))
        (lambda () (equal? (lid '(* 1 (+ 2 3)) 1)
                      '(+ 2 3)))
        (lambda () (equal? (rid '(/ (+ x 2) 1) 1)
                      '(+ x 2)))
         (lambda () (equal? (linv '(+ (- 1) 1) 1)
                       0))
         (lambda () (equal? (rinv '(/ 5 5) 1)
                       1))

    )))

(run-tests)
