(defun has-list-p (lst)
   (if (null lst)
       nil
       (if (listp (car lst))
           t
           (has-list-p (cdr lst)))))