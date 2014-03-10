(defun camelize (str &optional capitalize)
  (let ((len (1- (length str))))
    (do* ((i 0 (1+ i))
          (last (schar str 0) (schar str (1- i)))
          (current (schar str i) (schar str i))
          (final (if (eql capitalize t)
                     (list (char-upcase (schar str 0)))
                   (list (char-downcase (schar str 0))))
                 (cond ((eql #\- current) final)
                       ((eql #\- last) (cons (char-upcase current) final))
                       (t (cons current final)))))
         ((eql i len) (coerce (reverse final) 'string)))))


(defun hyphenate (str &optional case)
  (unless (or (eql case :upper) (eql case :lower)) 'error)
  (let ((len (1- (length str))))
    (do* ((i 0 (1+ i))
          (last (schar str 0) (schar str (1- i)))
          (current (schar str 0) (schar str i))
          (result (list current)
                  (if (and (lower-case-p last) (upper-case-p current))
                      (list* current #\- result)
                    (cons current result))))
         ((eql i len) (if (eql case ':lower)
                          (string-downcase (coerce (reverse result) 'string))
                        (string-upcase (coerce (reverse result) 'string)))))))



(defun camelize (string &optional capitalize)
  (let ((i 0))
    (when (< i (length string))
      (when (eql (char string i) #\-)
        (incf i)
        (setf (char string i) (char-upcase (char string i))))
      (incf i))
    (unless (null capitalize)
      (setf (char string 0) (char-upcase (char string 0)))))
  (remove #\- string))

(defun hyphenate (camel-string &optional (case :upper))
  (with-output-to-string (result)
     (loop for c across camel-string
           with lower-case-before
           when (and lower-case-before
                     (upper-case-p c))
             do (princ "-" result)
           if (lower-case-p c)
             do (setf lower-case-before t)
           else
             do (setf lower-case-before nil)
           if (eql case :upper)  
             do (princ (char-upcase c) result)
           else 
             do (princ (char-downcase c) result))))
    