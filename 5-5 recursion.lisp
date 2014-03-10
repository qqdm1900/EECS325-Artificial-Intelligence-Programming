;;; work liupeicong
(defun preceders (x v &key (start 0) (end nil) (test #'eql))
  (if (zerop (length v))
      nil
    (preceder x v :start 1 :end end :test test)))

(defun preceder (x v &key (start 1) (end nil) (test #'eql))
  (let ((pos (position x v :start start :end end :test test)))
    (cond ((null pos) nil)
          (t (adjoin (elt v (1- pos)) (preceder x v :start (1+ pos) :end end :test test))))))

5-5
(defun preceders (x v)
  (if (zerop (length v)) nil (preceder x v)))

(defun preceder (x v &key (start 1))
  (let ((pos (position x v :start start)))
    (cond ((null pos) nil)
          (t (adjoin (elt v (1- pos)) (preceder x v :start (1+ pos)))))))