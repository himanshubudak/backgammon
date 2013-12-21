#lang racket


(define  (make-2d-vector r c initial)
  (build-vector r (lambda (x) (make-vector c initial))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (vector-set! (vector-ref vec r) c val))

(define-syntax lc
  (syntax-rules ( <- *)
    [(lc expr : var <- drawn-from)  (map (lambda (var) expr) drawn-from)]
    [(lc expr : * guard) (if guard (list expr) `())]
    [(lc expr : * guard qualifier ...) 
     (concat (lc (lc expr : qualifier ...) : guard))]
    [(lc expr : var <- drawn-from qualifier ...) 
     (concat (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))

(define (concat l)
  (if (null? l) '()
      (append (car l) (concat (cdr l)))))


(provide (all-defined-out))