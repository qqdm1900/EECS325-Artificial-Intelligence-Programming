;;;challenge 1 xiaotian

(defun make-change (amount &optional (coin-list '(25 10 5 1)))
  (let ((coin-lst nil))
    (values-list (mapcar #'(lambda (x) 
                             (multiple-value-setq (coin-lst amount) (floor amount x)))
                         coin-list))))


;;;not read not submit
(defun make-best-change (amount  &optional (coin-list '(25 10 5 1)))
  (values-list (nreverse (change-helper amount coin-list))))

(defun change-helper (amount coin-list &optional (result nil) (best nil))
  (do ((lst (get-coin-set amount coin-list) (cdr lst))
       (best-list best (change-helper
                       (- amount (* (car coin-list) (car lst))) (cdr coin-list)
                       (cons (car lst) result) best-list)))
      ((null lst) (get-result amount result best-list))))


(defun get-coin-set (amount coin-list)
  (cond ((null coin-list) nil)
        ((null (cdr coin-list)) (list (floor amount (car coin-list))))
        (t (get-coin-list amount coin-list))))

(defun get-coin-list (amount coin-list)
  (let* ((number (+ 1 (floor amount (car coin-list))))
         (count number))
    (mapcar #'(lambda (x) (decf count)) (make-list number))))
       
                                       
(defun get-result (amount result best-list)
  (cond ((null best-list) result))

        ((and (< (reduce #'+ result) (reduce #'+ (car best-list))) (= amount (cdr best-list)))
         result)
        (t (car best-list)))   