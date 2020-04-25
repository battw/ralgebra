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
            
    )))

(let ([results (run-tests)])
  (if (andmap identity results)
    (println "tree.rkt PASSED")
    (println (format "tree.rkt FAILED: ~a" results))))
