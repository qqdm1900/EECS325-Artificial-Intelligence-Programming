;;;not work
;;v 1.0 not working
(defun delete-car (lst)
  (lambda (x)
    (if (consp x)
        (setf lst (rest x)))))
;;v 2.0 extra ()
(defun delete-car (lst)
  ;(let ((y (rest x)))
  (rplaca lst (value-list (cdr lst)))
  (rplacd lst nil))

;;v 3.0
(defun delete-car (lst)
  (cond ((not (null (cdr lst)))
         (rplaca lst (car (cdr lst)))
         (rplacd lst (cdr (cdr lst))))
        (t nil)))


(defun delete-car (l)
  (cond ((null (cdr l)) (setf l nil))
        (t (setf (first l) (second l) (cdr l) (cddr l))
  l)


(defun delete-car (x)
(let ((y (rest x)))
(setf (rest x) nil)
(setf x y)))


(defun delete-car (l)
  (cond ((null (cdr l)) (setf l nil))
        ((not (listp (cdr l))) (setf l (cdr l)))
        (T (setf (first l) (second l) (cdr l) (cddr l) l))))


;;;final	
(defun delete-car (lst)
  (cond ((null (cdr lst)) nil)
        ((atom (cdr lst)) (cdr lst))
        (t (setf (car lst) (cadr lst) (cdr lst) (cddr lst)) lst))) 
