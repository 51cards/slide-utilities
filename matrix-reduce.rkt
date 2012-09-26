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
                           (ref (map cdr (reduce Mx)))))))
;(define (subeach Vx Mx) ())

(define (right-column Mx) (map car (map reverse Mx)))

(define (rref Mx) (apply-subtractions (ref Mx)))

(define (apply-subtractions Mx) (if (null? (cdr Mx))
                                    Mx 
                                    (append (list 
                                             (process-subtractions (append (list (car Mx)) 
                                                                           (reverse (cdr Mx)))) 
                                             (apply-subtractions (cdr Mx)))))) 

(define (process-subtractions Mx) (if (null? (cdr Mx))
                                      Mx
                                      (process-subtractions (append(list(scale-subtract 
                                                                         (car Mx) (cadr Mx)))
                                                                   (cddr Mx)))
                                   ))
(define (scale-subtract x y) (map - x (map
                                       (lambda (z) 
                                             (* z (scale-factor x (place-of-first-non-zero y 0)))) 
                                           y)
                              ));; this is the function that is returning the wrong values!
(define (place-of-first-non-zero L x) (if (= 0 (car L))
                                          (if (null? (cdr L)) null (place-of-first-non-zero (cdr L) (+ x 1)))
                                              x))
(define (scale-factor V x) (if (= 0 x)
                               (car V)
                               (scale-factor (cdr V) (- x 1)))) ;error in this function!
;(place-of-first-non-zero '(0 0 0 0 2 0) 0)

;(define (rref Mx)
;  ((define (rr Mx) (append (list (subeach (car Mx) (map (lambda (x) (append '(0) x))(rr (map cdr (cdr Mx)))))) (map (lambda (x) (append '(0) x)) (ref (map cdr (cdr Mx))))))
;  (rr (ref Mx))))

(ref '((1 3 9 27 5) (1 2 4 8 1) (1 1 1 1 8) (1 4 16 64 10)))
(rref '((1 3 9 27 5) (1 2 4 8 1) (1 1 1 1 8) (1 4 16 64 10))) ;doesn't crash, but doesn't work yet ; getting so much closer!


;'((1 3 9 27 5) (0 1 5 19 4) (0 0 1 6 5 1/2) (0 0 0 1 -1 2/3))
;'(((1 2 3 1 -2 5/6)) (((0 1 5 19 4)) (((0 0 1 6 5 1/2)) ((0 0 0 1 -1 2/3)))))
;> (scale-subtract '(1 3 1 4 8) '(0 1 0 1 2))
;'(1 2 1 3 6)
;> (place-of-first-non-zero '(0 1 0 1 2) 0)
;1
;> (scale-factor '(1 3 1 4 8) 1)
;1
;> (scale-factor '(1 3 1 4 8) 2
;                )
;1
;> ( map - '(1 2 3) '(4 5 6))
;'(-3 -3 -3)
;> (map - '(1 3 1 4 8) (map (lambda (z) (* z 3)) '(0 1 0 1 2)))
;'(1 0 1 1 2)
;> 