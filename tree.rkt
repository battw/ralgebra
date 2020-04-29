#lang racket
(provide leaf-foldr leaf-map leaf-mapi num-leafs)

(define (num-leafs expr)
  (leaf-foldr (lambda (x acc) (add1 acc)) 0 expr))

(define leaf-foldr
  (case-lambda
    [(f init t)
     (let rec ([t t] [acc init])
       (cond
         [(null? t) acc]
         [(not (list? t)) (f t acc)]
         [else (rec (car t) (rec (cdr t) acc))]))]
    [(f init t . ts)
     (let rec ([ts (cons t ts)] [acc init])
       (cond
         [(null? (car ts)) acc]
         [(not (list? (car ts))) (apply f `(,@ts ,acc))]
         [else (rec (map car ts) (rec (map cdr ts) acc))]))]))

(define leaf-map
  (case-lambda
    [(f t)
     (let rec ([f f] [t t])
       (cond
         [(null? t) '()]
         [(list? t) (cons (rec f (car t)) (rec f (cdr t)))]
         [else (f t)]))]
    [(f t . ts)
     (let rec ([f f] [ts (cons t ts)])
       (cond
         [(null? (car ts)) '()]
         [(not (list? (car ts))) (apply f `(,(car ts) ,@(cdr ts)))]
         [else (cons (rec f (map car ts)) (rec f (map cdr ts)))]))]))   
  
(define leaf-mapi
  (case-lambda
    [(f t)
    (let rec ([i 1] [t t])
      (cond
        [(null? t) '()]
        [(list? t) (cons (rec i (car t)) (rec (+ i (num-leafs (car t))) (cdr t)))]
        [else (f i t)]))]
    [(f t . ts)
     (let rec ([i 1] [ts (cons t ts)])
       (cond
         [(null? (car ts)) '()]
         [(not (list? (car ts))) (apply f i `(,(car ts) ,@(cdr ts)))]
         [else (cons
                (rec i (map car ts))
                (rec (+ i (num-leafs (car (car ts)))) (map cdr ts)))]))])) 
 
(define (branch-map f t)
  (letrec ([rec (lambda (t)
                (cond
                  [(null? t) '()]
                  [(list? t) (f (skip t))]))]
           [skip (lambda (l)
                 (cond
                   [(null? l) '()]
                   [(list? (car l)) (cons (rec (car l)) (skip (cdr l)))]
                   [else (cons (car l) (skip (cdr l)))]))])
  (rec t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-tests)
      (map
       (lambda (test) (apply test '())) 
       (list
        (lambda () (equal?
               (leaf-foldr cons '() '(a (b c) ((d e) f)))
               '(a b c d e f)))
         (lambda () (equal?
                (leaf-foldr
                 (lambda (x y acc) (cons (cons x y) acc))
                 '()
                 '(a (b c) ((d e) f)) '((1 1) (2 3) ((4 (5 5)) 6)))
                '((a 1 1) (b . 2) (c . 3) (d . 4) (e 5 5) (f . 6))))
         (lambda () (equal?
                (leaf-foldr
                 (lambda (x acc) (add1 acc))
                 0
                 '(* (+ 3  4) (* (* -1 q) (+ (* 2 1) s))))
                13))
         (lambda () (equal?
                (leaf-foldr
                 (lambda (x acc) (cons x acc))
                 '()
                 'a)
                '(a)))
         (lambda () (equal?
                (leaf-foldr
                 (lambda (x y acc) (cons (cons x y) acc))
                 '()
                 'a
                 1)
                '((a . 1))))
         (lambda () (equal?
                     (leaf-map
                      add1
                      '(1 (2 3) ((4 5) 6) 7))
                     '(2 (3 4) ((5 6) 7) 8)))
         (lambda () (equal?
                     (leaf-map
                      add1
                      '(1 (2 3) ((4 5) 6) 7))
                     '(2 (3 4) ((5 6) 7) 8)))
         (lambda () (equal?
                     (leaf-map
                      add1
                      '())
                     '()))
         (lambda () (equal?
                     (leaf-map
                      add1
                      3)
                     4))
         (lambda () (equal?
                     (leaf-map
                      + 
                      '(1 (2 3) ((4 5) 6) 7)
                      '(3 (2 1) ((0 -1) -2) -3))
                      '(4 (4 4) ((4 4) 4) 4)))
         (lambda () (equal?
                     (leaf-mapi
                      (lambda (i x) (+ i x)) 
                      '(1 (2 3) ((4 5) 6) 7))
                      '(2 (4 6) ((8 10) 12) 14)))
         (lambda () (equal?
                     (leaf-mapi
                      (lambda (i x y) (+ i x y)) 
                      '(1 (2 3) ((4 5) 6) 7)
                      '(3 (2 1) ((0 -1) -2) -3))
                      '(5 (6 7) ((8 9) 10) 11)))
                      
         (lambda () (equal?
                     (branch-map
                      (lambda (b) (match b [`(,l ,r) `(,r ,l)]))
                      '((1 2) ((3 (4 5)) (6 7))))
                     '(((7 6) ((5 4) 3)) (2 1))))
    )))

(let ([results (run-tests)])
  (if (andmap identity results)
    (println "tree.rkt PASSED")
    (println (format "tree.rkt FAILED: ~a" results))))
