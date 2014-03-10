;;;recursive version
(defun intersperse (obj lst)
  (cond ((null (cdr lst)) lst)
        (t (list* (car lst) obj (intersperse obj (cdr lst))))))

;;;iterative version
(defun intersperse (obj lst)
  (do ((i lst (cdr i))
       (ls nil (if (null (cdr i))
                   (cons (car i) ls)
                 (list* obj (car i) ls))))
       ((null i) (nreverse ls))))

Second Submit
;;;

;;;iterative version
(defun intersperse (obj lst)
  (do ((i lst (cdr i))
       (ls nil (list* obj (car i) ls)))
       ((null (cdr i))
        (if (null i) nil (nreverse (cons (car i) ls))))))

