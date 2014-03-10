(in-package #:cs325-user)

;;;ex 2-4
(defun greater (x y)
  (if  (> x y) x y))

;;;ex 2-7
(defun has-list-p (lst)
   (cond ((null lst)  nil)
         ((listp (car lst)) t)
         (t (has-list-p (cdr lst)))))


;;;ex 2-8 (a) Iteration
(defun print-dots (int)
  (do ((i int (1- i)))
      ((= i 0))
    (format t ".")))

;;;ex 2-8 (a) Recursion
(defun print-dots (int)
  (cond ((= int 0) nil)
        (t (format t ".") (print-dots (1- int)))))

;;;ex 2-8 (b) Iteration 
(defun get-a-count (lst)
  (do ((i lst (cdr i)) (n 0 (+ n (if (eql (car i) 'a) 1 0))))
      ((not i) n)))


;;;ex 2-8 (b) Recursion
(defun get-a-count (lst)
  (cond ((null lst) 0)
        ((eql 'a (car lst)) (1+ (get-a-count (cdr lst))))
        (t (get-a-count (cdr lst)))))


;;;ex 2-9 (a) 
;;;The problem is that the function remove does not really remove the nil elements from the original list. It returns a new list without nil elements, but doesn't make any change to the original list.
;;;Correct version:
(defun summit (lst)
  (apply #'+ (remove nil lst)))


;;;ex 2-9 (b)
;;;The problem is that there will be no end of this recursion.
;;;Correct version:
(defun summit (lst)
  (cond ((null lst) 0)
        ((null (car lst)) (summit (cdr lst)))
        (t (+ (car lst) (summit (cdr lst))))))