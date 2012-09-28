#lang racket

;;;;; Standalone functions - each works on its own and independently

(define (power-list x n)
  (if (< n 0) '()
      (cons 1
            (map (lambda (y)
                   (* x y))
                 (power-list x (- n 1))))))

(define (scale-first-line L)
  (map (lambda (x)
         (/ x (car (car L))))
       (car L)))

(define (place-of-first-non-zero L x)
  (if (= 0 (car L))
      (if (null? (cdr L))
          null
          (place-of-first-non-zero (cdr L) (+ x 1)))
      x))

(define (scale-factor V x)
  (if (= 0 x)
      (car V)
      (scale-factor (cdr V) (- x 1))))

;;;;; Functions based off of the standalone functions

(define (scale-subtract x y)
  (map -
       x
       (map (lambda (z) 
              (* z (scale-factor x (place-of-first-non-zero y 0))))
            y)))

(define (process-subtractions Mx)
  (if (null? (cdr Mx))
      Mx
      (process-subtractions (cons (scale-subtract (car Mx)
                                                          (cadr Mx))
                                  (cddr Mx)))))

(define (apply-subtractions Mx)
  (if (null? (cdr Mx))
      Mx 
      (append (process-subtractions (append (list (car Mx))
                                            (apply-subtractions (cdr Mx))))
              (apply-subtractions (cdr Mx)))))

(define (reduce L)
  (map (lambda (x) 
         (map (lambda (y z)
                (- y z))
              x 
              (map (lambda (zz)
                     (* zz (car x)))
                   (scale-first-line L))))
       (cdr L)))

(define (ref Mx)
  (if (= 0 (car (car Mx)))
      (ref (append (cdr Mx)
                   (list (car Mx))))
      (if (null? (cdr Mx))
          (list (scale-first-line Mx))
          (cons (scale-first-line Mx)
                (map (lambda (x)
                       (cons 0 x))
                     (ref (map cdr (reduce Mx))))))))

(define (rref Mx)
  (apply-subtractions (ref Mx)))

(define (coefficients points)
  (right-column (rref (map (lambda (pair-of-points)
                             (makeline pair-of-points (length points)))
                           points))))

(define (fit-function-to-points points)
  (lambda (x)
    (apply + (map *
                  (power-list x (- (length points) 1))
                  (coefficients points)))))



(define (makeline my-pair n)
  (append (power-list (car my-pair) n) (cdr my-pair)))



                        






(define (right-column Mx)
  (map car (map reverse Mx)))














;;; test functions to show this thing works;;;
(ref '((1 3 9 27 5) (1 2 4 8 1) (1 1 1 1 8) (1 4 16 64 10)))
(rref '((1 3 9 27 5) (1 2 4 8 1) (1 1 1 1 8) (1 4 16 64 10)))
(map (fit-function-to-points '((1 3) (2 4) (3 0))) '(0 1 2 3 4))
