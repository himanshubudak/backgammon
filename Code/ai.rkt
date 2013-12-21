#lang racket
(require "utilities.scm")
(require "proj-v2-gen-moves.scm")                                                          
                                                                   
 
 (define (~board-value B) 
   (define ~sd-value (lc (if (= (vector-ref B 25) 15) 300
                               (cond ((= (is-A B x) 1) (if (and (< 0 x) (< x 7)) 1
                                                        (if (and (< 0 x) (< x 7)) 1.8 0.3)))
                                     ((= (is-A B x) 2)  (if (and (< 0 x) (< x 7)) (* 0.5 (is-A B x))
                                                              (if (and (< 6 x) (< x 13)) (* 3.5  (is-A B x)) 
                                                                    (if (and (< 12 x) (< x 19)) (* 3.5  (is-A B x)) (* 5 (is-A B x))))))
                                     ((= (is-A B x) 3)  (if (and (< 0 x) (< x 7)) (* 2.1 (is-A B x))
                                                              (if (and (< 6 x) (< x 13)) (* 2.8  (is-A B x)) 
                                                                    (if (and (< 12 x) (< x 19)) (* 3  (is-A B x)) (* 3.9 (is-A B x))))))
                                     ((= (is-A B x) 4)  (if (and (< 0 x) (< x 7)) (* 2.2 (is-A B x))
                                                              (if (and (< 6 x) (< x 13)) (* 2.5  (is-A B x)) 
                                                                    (if (and (< 12 x) (< x 19)) (* 2.8  (is-A B x)) (* 3.5 (is-A B x))))))
                                     ((= (is-A B x) 5)  (if (and (< 0 x) (< x 7)) (* 2.1 (is-A B x))
                                                              (if (and (< 6 x) (< x 13)) (* 2.5  (is-A B x)) 
                                                                    (if (and (< 12 x) (< x 19)) (* 2.8  (is-A B x)) (* 3.2 (is-A B x))))))))
                                
                               
                                  : x <- '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24) 
                                                     * (and (not (empty? B x)) (equal? (car (vector-ref B x)) 'R))))
 
 (define ~check-bar (lc 1 : x <- '(1 2 3 4 5 6) * (and (not (empty? B x)) (equal? (car (vector-ref B x)) 'B) (< 1 (is-B B x)))))
 (define ~bar-value (let ((m (if (null? ~check-bar) 0 (foldr (λ(x y) (+ x y)) 0 ~check-bar)))
                          (n (if (bar-A? B) (bar-A? B) 0))) 
                      (if (<= m 3) (- 0 (* m (* 2 n)))
                          (- 0 (* (* 2 n) m)))))
    (define ~check-side (lc 1 : x <- '(13 14 15 16 17 18) * (and (not (empty? B x)) (equal? (car (vector-ref B x)) 'R) (< 1 (is-A B x)))))
 (define ~side-value  (let ((m (if (null? ~check-side) 0 (foldr (λ(x y) (+ x y)) 0 ~check-side))))
                        (if (= m 0) 0
                            m)))
   
 (define ~check-home (lc 1 : x <- '(19 20 21 22 23 24) * (and (not (empty? B x)) (equal? (car (vector-ref B x)) 'R) (< 1 (is-A B x)))))
 (define ~home-value  (let ((m (if (null? ~check-home) 0 (foldr (λ(x y) (+ x y)) 0 ~check-home)))
                          (n (if (bar-B? B) (bar-B? B) 0)))
                        (if (= n 0) (* 2 m)
                            (if (<= m 3) (+ 0 (* m (* 2 n)))
                                  (+ 0 (* (* 3 n)  m))))))
   
  (define ~home-close (lc (if (not (open-A? B)) 0 
                              (if (= x 20)   0.1
                                  (if (= x 21)   0.2
                                      (if (= x 22)   0.3
                                          (if (= x 23)   0.4 0.5)))))
                          : x <- '(19 20 21 22 23 24) * (and (not (empty? B x)) (equal? (car (vector-ref B x)) 'B) (< 1 (is-B B x)))))
         
 (let ((z (foldr (λ(x y) (+ x y)) 0 ~sd-value))
       (y (foldr (λ(x y) (+ x y)) 0 ~home-close))
       (n (if (open-A? B) 10 0))
       (m (* 4.5 (vector-ref B 25))))
   (+ z y n m ~bar-value ~home-value ~side-value))
   )
                        
 
 (provide (all-defined-out))