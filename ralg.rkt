#lang racket
(require racket/trace)


(define (commute expr)
  (match expr
    [`(+ ,a ,b) (list '+ b a)]
    [`(* ,a ,b) (list '* b a)]
    [_ (list 'does-not-commute expr)]
    ))

(define (distribute expr)
    (match expr
      [`(* ,a (+ ,b ,c)) (list '+ (list '* a b) (list '* a c))]
      [`(+ (* ,a ,b) (* ,a ,c)) (list '* a (list '+ b c))]
      [_ (list 'does-not-distribute expr)]
      ))


(define (associate expr)
  (match expr
    [`(+ (+ ,a ,b) ,c) (list '+ a (list '+ b c))]  
    [`(* (* ,a ,b) ,c) (list '* a (list '* b c))]
    [`(+ ,a (+ ,b ,c)) (list '+ (list '+ a b) c)]
    [`(* ,a (* ,b ,c)) (list '* (list '* a b) c)]
    [_ (list 'does-not-associate expr)]
    ))

(define (ident expr)
  (match expr
    [`(+ 0 ,a) a]
    [`(* 1 ,a) a]
    [_ (list 'isnt-left-identity expr)]
    ))


(define (inverse expr . op)
  ;; (if (null? op)
  (if (equal? op '())
      (match expr
        [`(+ (* -1 ,a) ,a) 0]
        [`(* (/ 1 ,a) ,a) 1]
        [_ (list 'isnt-inverse expr)])
      (match op
        ['(+) (list '+ (list '* -1 expr) expr)]
        ['(*) (list '* (list '/ 1 expr) expr)]
      )))

  
  


(define (fsub i f expr . args)
  ;; replace sub-expression at index i (preorder) with f(sub-expression)
  (if (= i 1)
      (apply f (cons expr args))
      (match expr
        [`(,op ,a, b) (list op (fsub (- i 1) f  a) (fsub (- i (+ (size a) 1)) f b ))]
        [`(,op ,a) (list op (fsub (- i 1) f a ))]
        [`,a a]
        [_ (list 'no-match-in-fsub expr)]
        )))



(define (size expr)
  ;; number of symbols in expr (not including brackets)
  (prefoldl
   (lambda (acc a expr) (+ 1 acc))
   0
   expr
   ))

(define (prefoldl f acc expr)
  ;; a left fold over an expression applying f in preorder
  (match expr
    [`(,op ,a ,b) (prefoldl f (prefoldl f (f acc op expr) a) b)]
    [`(,op ,a) (prefoldl f (f acc op expr) a)]
    [`,a (f acc a expr)]
    ))

    

(define (com i expr)
  (fsub i commute expr))

(define (dis i expr)
  (fsub i distribute expr))

(define (ass i expr)
  (fsub i associate expr))

(define (id i expr)
  (fsub i ident expr))

(define (inv i expr . op)
  (apply fsub (cons i (cons inverse (cons expr op)))))
 
(define (ev i expr)
  (fsub i eval expr))




(trace inv fsub inverse)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-tests)
      (map
       (lambda (test) (apply test '())) 
       (list
        (lambda () (equal? (com 1 '(* x y))
                      '(* y x)))
        (lambda () (equal? (dis 1 '(* x (+ y z)))
                      '(+ (* x y) (* x z))))
        (lambda () (equal? (ass 1 '(+ 1 (+ 2 3)))
                      '(+ (+ 1 2) 3)))
        (lambda () (equal? (id 1 '(* 1 (+ 2 3)))
                      '(+ 2 3)))
         (lambda () (equal? (inv 1 'x '+) 
                       '(+ (* -1 x) x)))
         (lambda () (equal? (inv 1 '(+ (* -1 2) 2))
                       0))

         (lambda () (equal? (size '(/ (- 3 (+ a 2)) (* (- x) (* q (* w 3)))))
                       14))
         (lambda () (equal? (com 8 '(+ (/ 3 4) (* (- q) (+ (* 2 1) s))))
                       '(+ (/ 3 4) (* (- q) (+ s (* 2 1))))))

    )))

(run-tests)

;; (com '(+ (/ 3 4) (* (- q) (+ r s))) 5)





