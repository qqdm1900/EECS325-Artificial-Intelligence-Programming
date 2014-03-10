(define-test improve-code
  (assert-equal
   '(defun foo (n) (incf n) (list n))
   (improve-code
    '(defun foo (n) (setq n (+ n 1)) (append (list n) nil))))
  )

(defparameter *rules*
  '(((setq ?x (+ ?x 1)) (incf ?x))
    ((+ ?x 1) (1+ ?x))
    ((append (list ?x) ?y) (cons ?x ?y))
    ((cons ?x nil) (list ?x))
    ))

(defun improve-code (code)
  (cond ((null code) nil)
        (t (let ((new-code (run-rules code)))
             (cond ((atom code) code)
                   (t (cons (improve-code (car code))
                            (improve-code (cdr code)))))))))
  
(defun run-rules (code &optional (rules *rules*))
  (cond ((null rules) code)
        (t
         (run-rules (reform (caar rules) (cadar rules) code)
                    (cdr rules)))))
  


(defun reform (p1 p2 x)
  (let ((lsts (match p1 x)))
    (if (null lsts) x (replace-vars p2 (car lsts)))))

(defun replace-vars (x lst)
  (let ((v (assoc x lst)))
    (cond ((not (null v)) (cdr v))
          ((atom x) x)
          (t (cons (replace-vars (car x) lst)
                   (replace-vars (cdr x) lst))))))