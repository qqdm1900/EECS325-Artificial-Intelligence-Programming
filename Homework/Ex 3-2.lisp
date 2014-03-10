(in-package #:cs325-user)

;;;Ex 3-2

(defun stable-union(a b)
  (let ((lst (reverse a)))
    (dolist (obj b)
      (when (null (member obj lst))
          (pushnew obj lst)))
    (reverse lst)))

(defun stable-intersection(a b)
  (let ((lst nil))
    (dolist (obj a)
      (when (member obj b)
          (pushnew (car (member obj b)) lst)))
    (reverse lst)))

(defun stable-set-difference(a b)
  (let ((lst nil))
    (dolist (obj a)
      (when (null (member obj b))
          (pushnew obj lst)))
    (reverse lst)))
