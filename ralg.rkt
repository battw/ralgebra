#lang racket
(require racket/trace)
(require racket/bool)
(require (file "tree.rkt"))


(define operators '(* + ^ -))

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
    [`(+ ,a (+ ,b ,c)) `(+ (+ ,a ,b) ,c)]
    [`(* (* ,a ,b) ,c) `(* ,a (* ,b ,c))]
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
  ;; replace sub-expression at index i with f(sub-expression)
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
  ;; number of atoms (symbols, operators, numbers) in expr 
  (tfoldr (lambda (x acc) (add1 acc)) 0 expr))

;; TODO variable renaming for names which occur in both expressions 
(define (match-bind ex1 ex2)    
  ;; Binds atoms in ex1 to corresponding expressions in ex2.
  ;; Throws errors if the structure of the expressions don't match.
  ;; Binds are returned that the list indexes of the bindings correspond
  ;; to their index in ex1. 
  (define (bind ex1 ex2)
    (tfoldr
     (lambda (x y acc) (cons (cons x y) acc))
     '()
     ex1
     ex2))

  (define (valid? bindings)
    ;; for each binding (l . r)
    ;;    l must be an atom and
    ;;       if l is a number then r is the same number
    ;;       if l is an operator then r is the same operator
    ;;       if l is a variable then r is a well formed algebraic expression
    (andmap
     (lambda (binding)
       (match binding
         [`(,l . ,r)
          #:when (and (atom? l)
                      (implies (number? l) (equal? l r))
                      (implies (operator? l) (equal? l r))
                      (implies (variable? l) (expression? r)))
          #t]
         [_ (raise-user-error (format "invalid binding: ~a" binding))]))
     bindings))

  (define (consistent? bindings)
    ;; for any two bindings, if (x . y) and (x . z) then y = z.
    ;; TODO formally define = in the above condition and in the implementation
    (andmap
     (lambda (binding)
       (match binding
         [`(,l . ,r)
          (implies (variable? l) 
                   (andmap
                    (lambda (bindingg)
                      (match bindingg
                        [`(,ll . ,rr)
                         #:when (implies (equal? ll l)
                                         (equal? rr r))
                         #t]
                        [_ (raise-user-error (format 
                                             "inconsistent bindings ~a and ~a"
                                             binding bindingg))]))          
                    bindings))]
         [_ (raise-user-error (format "inconsistent bindings ~a" bindings))]))
     bindings))

  (let ([bindings (bind ex1 ex2)])
    (if (and (valid? bindings) (consistent? bindings))
        bindings
        '())))
        
  
;; (define (substitute equality expr)
;;   (let ([binding (bind (lhs equality expr))])
    
  
  
(define (lhs expr)
  (match expr
    [`(,op ,l ,r) l]
    [_ (raise-user-error (format "~a has no left hand side" expr))]))

(define (rhs expr)
  (match expr
    [`(,op ,l ,r) r]
    [_ (raise-user-error (format "~a has no left hand side" expr))]))
  
(define (atom? expr)
  (or (symbol? expr) (number? expr)))
      
(define (operator? expr)
  (member expr operators))

(define (variable? expr)
  (and (symbol? expr) (not (operator? expr))))

;; TODO implement this
(define (expression? expr)
  #t)
    

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

        (lambda () (equal? (match-bind '(+ (* a g) (* b c)) '(+ (* f 2) (* y (* z q))))
                           '((+ . +) (* . *) (a . f) (g . 2) (* . *) (b . y) (c * z q))))
        (lambda () (equal? #t
                           (with-handlers ([exn:fail:user? (lambda (exn) #t)])
                             (match-bind '(+ x 1) '(+ x 2)))))
        (lambda () (equal? #t
                           (with-handlers ([exn:fail:user? (lambda (exn) #t)])
                             (match-bind '(+ x x) '(+ 2 3)))))
        (lambda () (equal? #t
                           (with-handlers ([exn:fail:user? (lambda (exn) #t)])
                             (match-bind '(+ x x) '(+ (+ 2 3) (+ 3 2))))))
    )))
(let ([results (run-tests)])
  (if (andmap identity results)
    (println "ralg.rkt PASSED")
    (println (format "ralg.rkt FAILED: ~a" results))))
