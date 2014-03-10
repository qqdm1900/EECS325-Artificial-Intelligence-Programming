(in-package :cs325-user)

(define-test match
  (assert-equal '(nil) (match 'a 'a))
  (assert-equal '(((?x . a))) (match '?x 'a))
  (assert-equal '(((?y . c) (?x . a))) (match '(?x b ?y) '(a b c)))
  (assert-equal nil (match '(?x ?y ?x) '(a b c)))
  (assert-equal '(((?y . b) (?x . a))) (match '(?x ?y ?x) '(a b a)))
  )

(define-test match-?fn
  (assert-true (match '(?number) 12))
  (assert-false (match '(?number) 'a))
  (assert-equal '(((?x . 11))) (match '(?x (?number)) '(11 12)))
  (assert-equal nil (match '(?x (?number)) '(a b)))
  (assert-true (match '(?or a b) 'a))
  (assert-true (match '(?or a b) 'b))
  (assert-false (match '(?or a b) 'c))
  (assert-equal '(((?x . 12))) (match '(?and (?number) ?x) 12))
  (assert-equal nil (match '(?and (?number) ?x) 'a))
  (assert-equal '(((?x . b))) (match '(?and (?or a b) ?x) 'b))
  )

(defun match (x y &optional (lsts '(nil)))
  (cond ((null lsts) nil)
        ((and (consp x) (?-p (car x)) (fboundp (car x)))
         (apply (car x) (cons y (cons lsts (cdr x)))))
        ((?-p x) (update-bindings x y lsts))
        ((eql x y) lsts)
        ((or (atom x) (atom y)) nil)
        (t (match (cdr x) (cdr y)
                  (match (car x) (car y) lsts)))))

(defun ?number (y lsts)
  (and (numberp y) lsts))

(defun ?or (y lsts &rest pats)
  (do ((pats pats (cdr pats))
       (rlsts nil (append (match (car pats) y lsts) rlsts)))
      ((null pats) rlsts)))

(defun ?and (y lsts &rest pats)
  (do ((pats pats (cdr pats))
       (rslts lsts (match (car pats) y rslts)))
      ((or (null rslts) (null pats)) rslts)))

(defun update-bindings (x y lsts)
  (collect #'(lambda (lst) (bind x y lst))
           lsts))

(defun bind (x y lst)
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