(in-package #:cs325-user)

(defun greater (x y)
  (if (> x y) x y))

(define-test fib 
  (assert-equal 0 (fib 0))
  (assert-equal 1 (fib 1))
  (assert-equal 1 (fib 2))
  (assert-equal 2 (fib 3))
  (assert-equal 3 (fib 4))
  )

(defun fib (n)
  (if (< n 2) n 
    (+ (fib (- n 1)) (fib (- n 2)))))