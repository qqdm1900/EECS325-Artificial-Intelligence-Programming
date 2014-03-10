(in-package #:cs325-user)


;;; Test-driven development!
(define-test match
  (assert-true (match '? 'a))
  (assert-true (match '? 12))
  (assert-true (match '? '(a b c)))
  (assert-true (match '? nil))

  (assert-true (match 'a 'a))
  (assert-false (match 'a 'b))

  (assert-true (match '(?) '(a)))
  (assert-true (match '(? b ?) '(a b c)))
  (assert-false (match '(?) '(a b )))
  (assert-false (match '(? ?) '((a b))))

  (assert-false (match '(?) '()))
  )

(defun match (x y)
  (cond ((eql x '?) t)
        ((eql x y) t)
        ((atom x) nil)
        ((atom y) nil)
        (t
         (and (match (car x) (car y))
              (match (cdr x) (cdr y))))))

;;; A new problem... find if a pattern occurs anywhere
;;; in a nested list

(define-test find-match
  (assert-true (find-match 'nil *test-function*))
  (assert-true (find-match '? *test-function*))
  (assert-true (find-match '(setq hash (gethash ? ?)) *test-function*))
  (assert-false (find-match '(setf hash (gethash ? ?)) *test-function*))
  )

#| another cliffhanger!
(defun find-match (pat code)
  (cond ((eql pat code) t)
        ((atom pat) nil)
        ???
|#

// from http://grokcode.com/12/how-to-write-original-jokes-or-have-a-computer-do-it-for-you/

(defconstant *test-function*
  '(defun make-compound (word1 word2 &key POS)
  (let ((ho-list1 (cons word1
                        (cond ((gethash word1 *vocab*) (mapcar 'word-prop-literal 
                                                               (word-prop-homophone (gethash word1 *vocab*)))))))
    (ho-list2 (cons word2
                        (cond ((gethash word2 *vocab*) (mapcar 'word-prop-literal 
                                                               (word-prop-homophone (gethash word2 *vocab*)))))))
    (answer nil))
    
    (cond ((and (not (null (gethash word2 *vocab*))) (is-POS POS (gethash word2 *vocab*))) 
       (dolist (h1 (cdr ho-list1))
             (cond ((and (starts-with word2 h1) (> (length word2) (length h1)))
                    (cond ((char= (aref (subseq word2 (length h1)) 0) #\space)  ; there is a space at the break point
                           (setq answer (make-word-prop :literal (format nil "~O~O" word1 (subseq word2 (length h1)))
                                                        :POS (word-prop-POS (gethash word2 *vocab*))
                                                        :anim (word-prop-anim (gethash word2 *vocab*)))))
                          (t 
                           (setq answer (make-word-prop :literal (format nil "~O-~O" word1 (subseq word2 (length h1)))
                                                        :POS (word-prop-POS (gethash word2 *vocab*))
                                                        :anim (word-prop-anim (gethash word2 *vocab*)))))))))))
                               
           
    (cond ((and (not (null (gethash word1 *vocab*))) (is-POS POS (gethash word1 *vocab*))) 
       (dolist (h2 (cdr ho-list2))
             (cond ((and (starts-with word1 h2) (> (length word1) (length h2)))
                    (cond ((char= (aref (subseq word1 (length h2)) 0) #\space) ; there is a space at the break point
                           (setq answer (make-word-prop :literal (format nil "~O~O" word2 (subseq word1 (length h2)))
                                                        :POS (word-prop-POS (gethash word1 *vocab*))
                                                        :anim (word-prop-anim (gethash word1 *vocab*)))))
                          (t 
                           (setq answer (make-word-prop :literal (format nil "~O-~O" word2 (subseq word1 (length h2)))
                                                        :POS (word-prop-POS (gethash word1 *vocab*))
                                                        :anim (word-prop-anim (gethash word1 *vocab*)))))))))))

    (dolist (h1 ho-list1)
      (dolist (h2 ho-list2)
        (cond ((null answer)
               (let ((hash nil))
                 
                 (setq hash (gethash (format nil "~O and ~O" h1 h2) *vocab*))
                 (cond ((and hash (is-POS POS hash))
                        (setq answer (make-word-prop :literal (format nil "~O and ~O" word1 word2)
                                                     :POS (word-prop-POS hash)
                                                     :anim (word-prop-anim hash)))))
                 
                 (setq hash (gethash (format nil "~O and ~O" h2 h1) *vocab*))
                 (cond ((and hash (is-POS POS hash))
                        (setq answer (make-word-prop :literal (format nil "~O and ~O" word2 word1)
                                                     :POS (word-prop-POS hash)
                                                     :anim (word-prop-anim hash)))))
                   
                 (setq hash (gethash (format nil "~O~O" h1 h2) *vocab*))
                 (cond ((and hash (is-POS POS hash))
                        (setq answer (make-word-prop :literal (format nil "~O~O" word1 word2)
                                                     :POS (word-prop-POS hash)
                                                     :anim (word-prop-anim hash)))))
                 
                 (setq hash (gethash (format nil "~O~O" h2 h1) *vocab*))
                 (cond ((and hash (is-POS POS hash))
                        (setq answer (make-word-prop :literal (format nil "~O~O" word2 word1)
                                                     :POS (word-prop-POS hash)
                                                     :anim (word-prop-anim hash)))))

                 (setq hash (gethash (format nil "~O ~O" h1 h2) *vocab*))
                 (cond ((and hash (is-POS POS hash))
                        (setq answer (make-word-prop :literal (format nil "~O ~O" word1 word2)
                                                     :POS (word-prop-POS hash)
                                                     :anim (word-prop-anim hash)))))
                 
                 (setq hash (gethash (format nil "~O ~O" h2 h1) *vocab*))
                 (cond ((and hash (is-POS POS hash))
                        (setq answer (make-word-prop :literal (format nil "~O ~O" word2 word1)
                                                     :POS (word-prop-POS hash)
                                                     :anim (word-prop-anim hash))))))))))

    answer)))