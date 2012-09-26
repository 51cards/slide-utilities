#lang racket
;(append '(1) '(3))
(define M (list (list 2 4 5 1) (list 1 4 2 0) (list 3 -2 2 1)))


(define (scale-first-line L) (map  (lambda (x) (/ x (car (car L)))) (car L)))
(define (reduce L) (map (lambda (x) 
                          (map (lambda (y z) (- y z)) x 
                               (map (lambda (zz)
                                      (* zz (car x)))
                                    (scale-first-line L))))
                        (cdr L)))

;t(scale-first-line M)

(define (ref Mx) (if (null? (cdr Mx))  (list (scale-first-line Mx))
                     (append 
                      (list (scale-first-line Mx))
                      (map (lambda (x)
                             (append '(0) x))
                           (ref (map cdr (reduce Mx))))
                      
                      )
                     ))
;(define (subeach Vx Mx) ())

(define (place-of-first-non-zero L x) (if (= 0 (car L))
                                          (if (null? (cdr L)) null (place-of-first-non-zero (cdr L) (+ x 1)))
                                              x))
(define (scale-factor V x) (if (= 0 x)
                               (car V)
                               (scale-factor V (- x 1))))
(place-of-first-non-zero '(0 0 0 0 2 0) 0)

;(define (rref Mx)
;  ((define (rr Mx) (append (list (subeach (car Mx) (map (lambda (x) (append '(0) x))(rr (map cdr (cdr Mx)))))) (map (lambda (x) (append '(0) x)) (ref (map cdr (cdr Mx))))))
;  (rr (ref Mx))))

(ref '((1 3 9 27 5) (1 2 4 8 1) (1 1 1 1 8) (1 4 16 64 10)))