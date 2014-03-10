;;;recursion
(defun diff-by-one-p (lst)
  (cond ((null (cdr lst)) t)
        ((and
          (/= 1 (- (first lst) (second lst)))
          (/= -1 (- (first lst) (second lst))))
         (return-from diff-by-one-p nil))
        (t (diff-by-one-p (cdr lst)))))

;second submit!!
;;;recursion
(defun diff-by-one-p (lst)
  (cond ((null (cdr lst)) t)
        ((/= 1 (abs (- (first lst) (second lst)))) nil)
        (t (diff-by-one-p (cdr lst)))))


;;;do
(defun diff-by-one-p (lst)
  (do ((ls (cdr lst) (cdr ls))
       (obj (car lst) (car ls)))
      ((null ls) t)
    (when (and
           (/= 1 (- obj (car ls)))
           (/= -1 (- obj (car ls))))
      (return nil))))

;;;second submit!!!

;;;do
(defun diff-by-one-p (lst)
  (do ((ls (cdr lst) (cdr ls))
       (obj (car lst) (car ls)))
      ((or (null ls) (/= 1 (abs (- obj (car ls))))) (null ls))))

;;;mapc return
(defun diff-by-one-p (lst)
  (mapc #'(lambda (x y)
            (when (and (/= 1 (- x y)) (/= -1 (- x y)))
              (return-from diff-by-one-p nil)))
        lst (cdr lst))
  t)


;;;every
(defun diff-by-one-p (lst)
  (every #'(lambda (x y) (or (= 1 (- x y)) (= -1 (- x y))))
         lst (cdr lst))) 



;;;do-not good-critique: nth is expensive
(defun diff-by-one-p (lst)
  (cond ((null (cdr lst)) t)
  (t (do ((i 1 (1+ i))
          (j 0 i))
         ((= i (length lst)) t)
       (when (and
              (/= 1 (- (nth i lst) (nth j lst)))
              (/= -1 (- (nth i lst) (nth j lst))))
         (return nil))))))