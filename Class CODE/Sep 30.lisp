(in-package #:cs325-user)

;;; Load day-1.lisp before loading this code

;;; Is recursion slow?
;;;
;;; The canonical recursive function, factorial
;;;
;;; Try (fact 100)

(define-test fact
  (assert-equal 1 (fact 1))
  (assert-equal 120 (fact 5))
  )

(defun fact (n)
  (if (< n 2) 1
    (* n (fact (- n 1)))))

;;; Tests for a recursive linear time fibonacci function
(define-test fib2
  (dotimes (i 20)
    (assert-equal (fib i) (fib2 i) i)))

;;; Use multiple values to return the current and preceding value
;;; Try (fib2 100) and see how it runs
;;; See chapter 5 for more on multiple values in Lisp
(defun fib2 (n)
  (if (< n 2) (values n 0)
    (multiple-value-bind (n1 n2) (fib2 (- n 1))
      (values (+ n1 n2) n1))))

;;; New topic: a symbolic pattern matcher

;;; Test-driven development!
(define-test match
  (assert-true (match '? 'a))
  (assert-true (match '? 12))
  (assert-true (match '? '(a b c)))
  (assert-true (match '? nil))

  (assert-true (match 'a 'a))
  (assert-false (match 'a 'b))

  (assert-true (match '(?) '(a)))
  (assert-true (match '(? b ?) '(a b c)))
  (assert-false (match '(?) '(a b )))
  (assert-false (match '(? ?) '((a b))))
  )

#| need to write...

(defun match (pat x)
  (cond ((eql pat '?) 

|#