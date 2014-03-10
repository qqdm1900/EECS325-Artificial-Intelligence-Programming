(defstruct (tconc (:constructor make-new-tconc))
  (head nil)
  (tail nil))

(defun make-tconc (&optional lst)
  (let ((tcnc (make-new-tconc)))
    (setf (tconc-head tcnc) lst
          (tconc-tail tcnc) (last lst))
    tcnc))
 

(defun tconc (tconc-structure &rest objs)
  (funcall #'tconc-list tconc-structure objs))


(defun tconc-list (tconc-structure &optional l)
  (unless (null l)
    (if (null (tconc-head tconc-structure))
        (setf (tconc-head tconc-structure) l)
      (setf (cdr (tconc-tail tconc-structure)) l))
    (setf (tconc-tail tconc-structure) (last l)))
  (tconc-head tconc-structure))



