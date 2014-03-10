(in-package :cs325-user)

(defun show-foo ()
  (flet ((show (x)
           (let ((z (foo (car x) (cadr x))))
             (format t "~%~S and ~S => ~S"  (car x) (cadr x) z))))
    (mapc #'show
          '((?x a)
            ((?x b ?y) (a b c))
            ((?x ?y ?x) (a b c))
            ((?x ?y ?x) (a b a))))
    t))

(defun foo (x y &optional (lsts '(nil)))
  (cond ((null lsts) nil)
        ((?-p x)
         (collect #'(lambda (lst) (baz x y lst))
                  lsts))
        ((eql x y) lsts)
        ((or (atom x) (atom y)) nil)
        (t (foo (cdr x) (cdr y)
                (foo (car x) (car y) lsts)))))

(defun baz (x y lst)
  (let ((v (assoc x lst)))
    (cond ((null v) (cons (cons x y) lst))
          ((equal (cdr v) y) lst)
          (t nil))))

(defun ?-p (x)
  (and (symbolp x)
       (eql (char (symbol-name x) 0) #\?)))

(defun collect (fn lst)
  (loop for x in lst
        when (funcall fn x)
        collect it))