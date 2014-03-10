;;;not pass
;;;not done
(defun max-min (v &optional (max nil)  (min nil) &key (start 0) (end (length v)))
  (cond ((or (null v) (= start end))
         (values max min))
        (t (let* ((x (svref v start))
                  (max (if (or (null max) (> x max)) x max))
                  (min (if (or (null min) (< x min)) x min)))
             (max-min v max min :start (1+ start) :end end)))))


;;;submit almost done
(defun max-min (v &key (max nil) (min nil) (start 0) (end (length v)))
  (cond ((or (null v) (= start end)) (values max min))
        (t (let* ((x (svref v start))
                  (max (if (or (null max) (> x max)) x max))
                  (min (if (or (null min) (< x min)) x min)))
                 (max-min v :max max :min min :start (1+ start) :end end)))))

;;;liupeicong
(defun max-min (v &key (start 0) (end (length v)) (max nil) (min nil))
  (cond ((or (null v) (= start end)) (values max min))
        (t (let* ((current (svref v start))
                  (max (if (or (null max) (> current max))
                           current
                         max))
                  (min (if (or (null min) (< current min))
                           current
                         min)))
                 (max-min v :start (1+ start) :end end :max max :min min)))))'