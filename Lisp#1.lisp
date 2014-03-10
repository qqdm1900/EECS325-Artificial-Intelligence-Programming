(defun has-number-p (s-exp)
  (if (atom s-exp)
      (numberp s-exp)
    (some #'has-number-p s-exp)))




;;;
(defun has-number-p (lst)
  (cond ((atom lst) (numberp lst))
        (t (or
	(some #'numberp lst)
	(has-number-p (car lst))
	(has-number-p (cdr lst))))))
