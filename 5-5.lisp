
;;;improved version iterative
(defun preceders (x v)
  (let ((lst nil))
    (dotimes (i (- 1 (length v)))
      (when (eql x (elt v (1+ i)))
        (push (elt v (1- i)) lst)))
    (remove-duplicates lst)))

----------------------------------------------------------------------
If V is a list, not a vector, don't use = and LENGTH. LENGTH has to
CDR down the entire list. Use (NULL (CDR ...)) with the appropriate
number of CDR's. That will run in constant time, independent of list
length.
----------------------------------------------------------------------
;;;iterative
(defun preceders (x v)
  (let ((lst nil))
    (dotimes (i (length v))
      (when (and (eql x (elt v i)) (< 0 i))
        (push (elt v (1- i)) lst)))
    (remove-duplicates lst)))

;;;recursive
(defun preceders (x v)
  (cond ((< (length v) 2) nil)
        ((eql x (elt v 1))
         (remove-duplicates (cons (elt v 0) (preceders x (subseq v 1)))))
        (t (preceders x (subseq v 1)))))

;Second Submit
;;;iterative length not good
(defun preceders (x v)
  (do ((i 1 (1+ i))
       (lst nil (if (eql x (elt v i)) (adjoin (elt v (1- i)) lst) lst)))
      ((>= i (length v)) lst)))

;;;iterative length better!!!
(defun preceders (x v)
  (do ((i (1- (length v)) (1- i))
       (lst nil (if (eql x (elt v i)) (adjoin (elt v (1- i)) lst) lst)))
      ((< i 1) lst)))

second submitt
;;;recursive not work
(defun preceders (x v &key (start 1))
  (cond ((null (find x v)) nil)
        ((eql x (elt v 1))
          (adjoin (elt v 0) (perceders x v :start (position x v))))
        (t (preceders x v :start (position x v)))))

;;;??? not work...       
(defun preceders (obj v &key start)
  (if (null (position obj v)) nil
  (adjoin
   (find-if #'(lambda (x) (eql obj (elt v (1+ (position x v))))) v)
   (preceders obj v :start (position obj v)))))


;;;final
(defun preceders (x v)
  (if (zerop (length v)) nil (preceder x v)))

(defun preceder (x v &key (start 1))
  (let ((pos (position x v :start start)))
    (cond ((null pos) nil)
          (t (adjoin (elt v (1- pos)) (preceder x v :start (1+ pos)))))))