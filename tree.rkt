#lang racket


(define tfoldr
  (case-lambda
    [(f init t)
     (let rec ([acc init] [t t])
       (cond
         [(null? t) acc]
         [(list? (car t)) (rec (rec acc (cdr t)) (car t))]
         [else (f (car t) (rec acc (cdr t)))]))]
    [(f init t . ts)
     (let rec ([acc init] [ts (cons t ts)])
       (cond
         [(null? (car ts)) acc]
         [(list? (car (car ts)))
          (rec (rec acc (map cdr ts)) (map car ts))]
         [else (apply f `(,@(map car ts) ,(rec acc (map cdr ts))))]
         ))]))
    
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
    )))

(let ([results (run-tests)])
  (if (andmap identity results)
    (println "tree.rkt PASSED")
    (println (format "FAILED: ~a" results))))

