(defmacro key-if (test &rest exprs)
  (let ((then (items-between :then :else exprs))
        (else (items-between :else :then exprs)))
    `(cond (,test ,@then)
           (t ,@else))))

(defun items-between (marker1 marker2 list)
  (let ((lst (cdr (member marker1 list))))
    (if (null lst)
        '(nil)
      (ldiff lst (member marker2 lst)))))

