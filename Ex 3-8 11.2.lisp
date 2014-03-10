

;;;show-dots
(defun show-dots(lst)
  (cond ((atom lst) (format t "~A" lst))              
        (t (format t "(")
           (show-dots (car lst))
           (format t " . ")
           (show-dots (cdr lst))
           (format t ")"))))


;;;show-list
(defun show-list (lst)
  (cond ((atom lst) (format t "~A" lst))
        (t (format t "[")
           (show-element lst)
           (format t "]"))))

(defun show-element (lst)
  (cond ((null lst) nil)
        (t (show-list (car lst))
           (when (cdr lst)
             (format t " "))
           (if (listp (cdr lst))
               (show-element (cdr lst))
             (format t ". ~A" (cdr lst))))))


;;;[ ]
(defun show-list (lst)
  (cond ((atom lst) (format t "~A" lst))
        (t (format t "[")
           (show-list (car lst))
           (show-element (cdr lst))
           (format t "]"))))

(defun show-element (lst)
  (cond ((null lst) nil)
        ((atom lst) (format t " . ~A" lst))
        (t (format t " ")
           (show-list (car lst))
           (show-element (cdr lst)))))

