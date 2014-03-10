(defun improve-code (code)
  (cond ((null code) nil)
        (t (run-rules 
             (cond ((atom code) code)
                   (t (cons (improve-code (car code))
                            (improve-code (cdr code)))))))))



(defparameter *rules*
  '(((setq ?x (1+ ?x)) (incf ?x))
    ((+ ?x 1) (1+ ?x))
    ((append (list ?x) ?y) (cons ?x ?y))
    ((cons ?x nil) (list ?x))
    ((typep ?x 'integer) (integerp ?x))
    ((car (nthcdr ?x ?lst)) (nth ?x ?lst))
    ((cons (cons ?x ?y) ?lst) (acons ?x ?y ?lst))
    ((if ?test t nil) ?test)
    ((car (member ?x ?lst :key 'car)) (assoc ?x ?lst))
    ((do ?clauses (?test (reverse ?lst)))
     (do ?clauses (?test (nreverse ?lst))))
    ))


(define-test improve-code
  (assert-equal
   '(defun foo (n) (incf n) (list n))
   (improve-code '(defun foo (n) (setq n (+ n 1)) (append (list n) nil))))
  (assert-equal
   '(integerp n)
   (improve-code '(if (typep n 'integer) t nil)))
  (assert-equal
   '(list (nth n items))
   (improve-code '(list (car (nthcdr n items)))))
  (assert-equal
   '(assoc key pairs)
   (improve-code '(car (member key pairs :key 'car))))
  )