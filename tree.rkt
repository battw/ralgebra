#lang racket
(provide tfoldr)

(define tfoldr
  (case-lambda
    [(f init t)
     [let rec ([t t] [acc init])
       (cond
         [(null? t) acc]
         [(not (list? t)) (f t acc)]
         [else (rec (car t) (rec (cdr t) acc))])]]
    [(f init t . ts)
     [let rec ([ts (cons t ts)] [acc init])
       (cond
         [(null? (car ts)) acc]
         [(not (list? (car ts))) (apply f `(,@ts ,acc))]
         [else (rec (map car ts) (rec (map cdr ts) acc))])]]))

(define (tmap f t)
  (cond
    [(null? t) '()]
    [(list? t) (cons (tmap f (car t)) (tmap f (cdr t)))]
    [else (f t)]))
  
(define (tmapi f t)
  (let rec ([i 1] [t t])
    (cond
      [(null? t) '()]
      [(list? t) (cons (rec i (car t)) (rec (+ i (tsize (car t))) (cdr t)))]
      [else (f i t)])))
 
(define (tsize expr)
  (tfoldr (lambda (x acc) (add1 acc)) 0 expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-tests)
      (map
       (lambda (test) (apply test '())) 
       (list
        (lambda () (equal?
               (tfoldr cons '() '(a (b c) ((d e) f)))
               '(a b c d e f)))
         (lambda () (equal?
                (tfoldr
                 (lambda (x y acc) (cons (cons x y) acc))
                 '()
                 '(a (b c) ((d e) f)) '((1 1) (2 3) ((4 (5 5)) 6)))
                '((a 1 1) (b . 2) (c . 3) (d . 4) (e 5 5) (f . 6))))
         (lambda () (equal?
                (tfoldr
                 (lambda (x acc) (add1 acc))
                 0
                 '(* (+ 3  4) (* (* -1 q) (+ (* 2 1) s))))
                13))
         (lambda () (equal?
                (tfoldr
                 (lambda (x acc) (cons x acc))
                 '()
                 'a)
                '(a)))
         (lambda () (equal?
                (tfoldr
                 (lambda (x y acc) (cons (cons x y) acc))
                 '()
                 'a
                 1)
                '((a . 1))))
         (lambda () (equal?
                     (tmap
                      add1
                      '(1 (2 3) ((4 5) 6) 7))
                     '(2 (3 4) ((5 6) 7) 8)))
         (lambda () (equal?
                     (tmap
                      add1
                      '())
                     '()))
         (lambda () (equal?
                     (tmap
                      add1
                      3)
                     4))
         (lambda () (equal?
                     (tmapi
                      (lambda (i x) (if (= i 3) 'Q x))
                      '(1 (2 3) ((4 5) 6) 7))
                      '(1 (2 Q) ((4 5) 6) 7)))
                      
    )))

(let ([results (run-tests)])
  (if (andmap identity results)
    (println "tree.rkt PASSED")
    (println (format "tree.rkt FAILED: ~a" results))))
