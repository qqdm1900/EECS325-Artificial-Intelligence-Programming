9-6
;;;lei
(defun horner (x-value &rest parameters)
  (reduce #'(lambda (x y) (+ (* x-value x) y)) parameters))

;;;kaopu
(defun horner (x &rest coefficients)
  (reduce #'(lambda (coef1 coef2)
              (+ (* coef1 x) coef2))
          coefficients))