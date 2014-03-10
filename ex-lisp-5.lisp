;;;not read yet!!! submit

(defun collect-numbers (lst)
  (cond ((numberp lst) (list lst))
        ((atom lst) nil)
        (t (mapcan (function collect-numbers) lst))))