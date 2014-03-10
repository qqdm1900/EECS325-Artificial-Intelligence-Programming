(in-package :cs325-user)

;;; The quiz code
(defun foo (x y)
  (cond ((eql x y) t)
        ((atom x) nil)
        ((eql (car x) '?*)
         (or (foo (cdr x) y)
             (and (consp y) (foo x (cdr y)))))
        ((atom y) nil)
        (t
         (and (foo (car x) (car y))
              (foo (cdr x) (cdr y))))))


;;; the quiz code integrated into the matcher

(defun match (x y)
  (cond ((eql x '?) t)
        ((eql x y) t)
        ((atom x) nil)
        ((eql (car x) '?*)
         (or (match (cdr x) y)
             (and (consp y) (match x (cdr y)))))
        ((atom y) nil)
        (t
         (and (match (car x) (car y))
              (match (cdr x) (cdr y))))))

;;; some tests illustrating what the new code does
;;; load the day 3 code and this and do
;;;   (run-tests match find-match match-?*)
;;; to verify everything works

(define-test match-?*
  (assert-true (match '(?*) '()))
  (assert-true (match '(?*) '(a b c d)))
  (assert-true (match '(?* b ?*) '(a b c d)))
  (assert-false (match '?* '()))
  (assert-true (match '(? ?*) '(a)))
  (assert-true (match '(? ?*) '(a b c d)))
  (assert-false (match '(? ?*) '()))
  ;;; a pattern true only for a list with a sublist with at least 2 elements
  (assert-true (match '(?* (? ? ?*) ?*) '(a b (c) (d e) ())))
  (assert-false (match '(?* (? ? ?*) ?*) '(a b (c) (d) ())))
  )
  



(defun find-match (pat code)
  (or (match pat code)
      (and (consp code)
           (or (find-match pat (car code))
               (find-match pat (cdr code))))))