;;;lei
(defun make-change (amount &optional (coin-list '(25 10 5 1)))
  (let ((coin-lst nil))
    (values-list (mapcar #'(lambda (x) 
                             (multiple-value-setq (coin-lst amount) 
                                 (floor amount x))) coin-list))))


;;;lei not good enough
(defun make-change(payment &optional (coins '(25 10 5 1)))
  (do ((coin coins (rest coin))
       (owe-pay payment (mod owe-pay (car coin)))
       (result nil (append result
                          (list (floor owe-pay (car coin))))))
      ((null coin) (values-list result))))

;;;another kaopu version
(defun make-change (x &optional lst)
  (cond ((null x) (values 0 0 0 0))
        ((null lst) (values-list (get-coins x '(25 10 5 1))))
        (t (values-list (get-coins x lst)))))

(defun get-coins (x lst)
  (cond ((null x) nil)
        ((null lst) nil)
        (t (let ((l (multiple-value-bind (y z)
                        (truncate x (car lst))
                      (list y z))))
             (cons (car l) (get-coins (cadr l) (cdr lst)))))))