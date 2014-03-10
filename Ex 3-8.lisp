;;;many problems....
(defun show-dots(lst)
  (cond ((atom lst) lst)
        ((not (listp (cdr lst))) (format t "~A)" (cdr lst)))
        ((null lst) (format t "NIL)"))
        ((atom (car lst)) (format nil "(~A . ~A)" (car lst) (show-dots(cdr lst))))
        (t (show-dots(car lst)) (show-dots(cdr lst)))))

;;;version 2 with problems
(defun show-dots (lst)
  (if (null (cdr lst))
      (format t "(~A . ~A)" (car lst) "NIL")
      (progn
	(format t "(~A . " (car lst))
	(show-dots (cdr lst))
	(format t ")"))))



;;Correct version but I don't know why....
(defun show-dots(ls)
  (format t "~A" (get-dots ls)))

(defun get-dots(ls)
  (if (atom ls)
      ls
    (format nil "(~A . ~A)" (get-dots (car ls)) (get-dots (cdr ls)))))


;;;wrong...dolist don't make sense for (a . c)
(defun show-list(lst)
  (cond ((atom lst) lst)
        (t (format t "[")
           (dolist (obj lst)
             (if (atom obj)
                 (format t "~A" obj)
               (show-list obj)))
           (format t "]"))))

;;;Correct!!!
(defun show-list (lst)
  (cond ((atom lst) (format t "~A" lst))
        (t (format t "[")
           (show-element lst)
           (format t "]"))))

(defun show-element (lst)
  (cond ((null lst) nil)
        ((atom (car lst))
         (format t "~A" (car lst))
         (unless (null (cdr lst))
           (format t " "))
         (if (listp (cdr lst))
             (show-element (cdr lst))
           (format t ". ~A" (cdr lst))))
        (t (show-list (car lst))
           (unless (null (cdr lst)) (format t " "))
           (show-element (cdr lst)))))

