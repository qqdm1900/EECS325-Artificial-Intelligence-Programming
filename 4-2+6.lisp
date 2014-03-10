;;;my-copy-list
(defun my-copy-list (lst)
  (reduce #'cons lst :from-end t :initial-value nil))

;;;my-reverse
(defun my-reverse (lst)
  (reduce #'(lambda (x y) (cons y x)) lst :initial-value nil))


;;;hash-table->alist
(defun hash-table->alist (ht)
  (let ((alst nil))
    (maphash #'(lambda (k v) (push (cons k v) alst)) ht)
    alst))

;;;alist->hash-table
(defun alist->hash-table (alst)
  (let ((ht (make-hash-table)))
    (mapc #'(lambda (x) (setf (gethash (car x) ht) (cdr x))) alst)
    ht))


(defun alist->hash-table (alst)
  (let ((ht (make-hash-table)))
    (dolist (x alst)
      (setf (gethash (car x) ht) (cdr x)))
    ht))
