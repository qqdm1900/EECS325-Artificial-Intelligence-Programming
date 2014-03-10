
;;;not test

(defun assert-file (file)
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil)
               (read-line str nil))
         (count 0 (1+ count)))
        ((null line) count)
      (cond ((find #\) line :test #'equal)
             (tell (trim-to-list line)))
            (t 
             (tell (trim-to-list
              (concatenate 'string line (read-line str nil)))))))))
     
(defun trim-to-list (stream)
  (with-input-from-string 
      (str (string-trim '(#\Space #\( #\)) stream))
    (loop for x = (read str nil 'eof)
          until (eq x 'eof)
          collect x)))