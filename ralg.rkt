#lang racket
(require racket/trace)


(define (commute expr)
  (match expr
    [`(+ ,a ,b) `(+ ,b ,a)]
    [`(* ,a ,b) `(* ,b ,a)]
    [_ (raise-user-error (format "~a does not commute" expr))]))

(define (distribute expr)
    (match expr
      [`(* ,a (+ ,b ,c))        `(+ (* ,a ,b) (* ,a ,c))]
      [`(+ (* ,a ,b) (* ,a ,c)) `(* ,a (+ ,b ,c))]
      [_ (raise-user-error (format "~a does not distribute" expr))]))


(define (associate expr)
  (match expr
    [`(+ (+ ,a ,b) ,c) `(+ ,a (+ ,b ,c))]  
    [`(* (* ,a ,b) ,c) `(* ,a (* ,b ,c))]
    [`(+ ,a (+ ,b ,c)) `(+ (+ ,a ,b) ,c)]
    [`(* ,a (* ,b ,c)) `(* (* ,a ,b) ,c)]
    [_ (raise-user-error (format "~a does not associate" expr))]))

(define (ident expr . op)
  (if (null? op)
      (match expr
        [`(+ 0 ,a) a]
        [`(* 1 ,a) a]
        [_ (raise-user-error (format "~a not identity" expr))])
      (match op
        ['(+) `(= ,expr (+ 0 ,expr))]
        ['(*) `(= ,expr (* 1 ,expr))]
        [_ (raise-user-error (format "~a doesn't have identity" expr))])))


(define (inverse expr . op)
  (if (null? op)
      (match expr
        [`(+ (* -1 ,a) ,a) 0]
        [`(* (^ ,a -1) ,a) 1]
        [_ (raise-user-error (format "~a not inverse" expr))])
      (match op
        ['(+) `(= 0 (+ (* -1 ,expr) ,expr))]
        ['(*) `(= 1 (* (^ ,expr -1) ,expr))]
        [_ (raise-user-error (format "~a doesn't have inverse" op))])))
  


(define (transform i f expr . args)
  ;; replace sub-expression at index i (preorder) with f(sub-expression)
  (if (= i 1)
      (apply f (cons expr args))
      (match expr
        [`(,op ,a, b) (list op
                            (transform (- i 1) f  a)
                            (transform (- i (+ (size a) 1)) f b ))]
        [`(,op ,a) (list op (transform (- i 1) f a ))]
        [`,a a]
        [_ (raise-user-error 'transform "no match for ~a" expr)])))



(define (size expr)
  ;; number of symbols in expr (not including brackets)
  (prefoldl
   (lambda (acc a expr) (+ 1 acc))
   0
   expr))

(define (prefoldl f acc expr)
  ;; a left fold over an expression applying f in preorder
  (match expr
    [`(,op ,a ,b) (prefoldl f (prefoldl f (f acc op expr) a) b)]
    [`(,op ,a) (prefoldl f (f acc op expr) a)]
    [`,a (f acc a expr)]))


(define (bind ex1 ex2)    
  ;; Binds atoms in ex1 to corresponding expressions in ex2.
  (match `(,ex1 . ,ex2)
    ['(() . ()) '()]
    [`((,x . ,xs) . (,y . ,ys))
     (append
      (bind x y)
      (bind xs ys))]
    [`(,ex1 . ,ex2) #:when (atom? ex1) `((,ex1 . ,ex2))]
    [_ (raise-user-error 'bind "cannot bind ~a and ~a" ex1 ex2)]))

  
(define (atom? expr)
  (or (symbol? expr) (number? expr)))
      

(define (com i expr)
  (transform i commute expr))

(define (dis i expr)
  (transform i distribute expr))

(define (ass i expr)
  (transform i associate expr))

(define (id i expr . op)
  (apply transform (cons i (cons ident (cons expr op)))))

(define (inv i expr . op)
  (apply transform (cons i (cons inverse (cons expr op)))))
 
(define (ev i expr)
  (transform i eval expr))




;; (trace inv transform inverse)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-tests)
      (map
       (lambda (test) (apply test '())) 
       (list
        (lambda () (equal? (com 1 '(* x y))
                      '(* y x)))
        (lambda () (equal? (com 1 '(+ (* 2 2) y))
                      '(+ y (* 2 2))))

        (lambda () (equal? (dis 1 '(* x (+ y z)))
                      '(+ (* x y) (* x z))))
        (lambda () (equal? (dis 1 '(+ (* x y) (* x z)))
                           '(* x (+ y z))))
        

        (lambda () (equal? (ass 1 '(+ 1 (+ 2 3)))
                      '(+ (+ 1 2) 3)))
        (lambda () (equal? (ass 1 '(+ (+ 1 2) 3))
                      '(+ 1 (+ 2 3))))
        (lambda () (equal? (ass 1 '(* 1 (* 2 3)))
                      '(* (* 1 2) 3)))
        (lambda () (equal? (ass 1 '(* (* 1 2) 3))
                      '(* 1 (* 2 3))))

        (lambda () (equal? (id 1 '(* 1 (+ 2 3)))
                      '(+ 2 3)))
        (lambda () (equal? (id 1 '(+ 0 (+ 2 3)))
                      '(+ 2 3)))
        (lambda () (equal? (id 1 '(+ 1 1) '+)
                      '(= (+ 1 1) (+ 0 (+ 1 1)))))
        (lambda () (equal? (id 1 '(+ 1 1) '*)
                      '(= (+ 1 1) (* 1 (+ 1 1)))))

         (lambda () (equal? (inv 1 '(+ (* -1 2) 2))
                       0))
         (lambda () (equal? (inv 1 '(* (^ 2 -1) 2))
                       1))
         (lambda () (equal? (inv 1 'x '+) 
                       '(= 0 (+ (* -1 x) x))))
         (lambda () (equal? (inv 1 'x '*) 
                       '(= 1 (* (^ x -1) x))))

         (lambda () (equal? (size '(* (* -1 (+ a 2)) (* (* -1 x) (* q (* w 3)))))
                       15))
         (lambda () (equal? (com 9 '(* (+ 3  4) (* (* -1 q) (+ (* 2 1) s))))
                       '(* (+ 3 4) (* (* -1 q) (+ s (* 2 1))))))

         (lambda () (equal? (bind '(+ (& a 2) (* b c)) '(* (/ f g) (- y (* z q))))
                       '((+ . *) (& . /) (a . f) (2 . g) (* . -) (b . y) (c * z q))))
    )))

(let ([results (run-tests)])
  (if (andmap identity results)
    (println "PASSED")
    (println (format "FAILED: ~a" results))))
