;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/explanation:explain.lisp,v 1.3.3.1 2011/08/24 13:25:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; --------------------- A SIMPLE EXPLANATION FACILITY ---------------------

(in-package kw-user)

; connects rule to explanation definitions
(defvar *explanation-table* (make-hash-table :test #'eq))

; explanation generated at runtime
(defvar *explanation* nil)

;;; the next four definitions make up the defexplain macro
;;; for each of the why, what and because definitions we create a
;;; function which we can call at runtime on the bindings of the
;;; instantiation to generate the explanation text - this will be
;;; reasonably efficient

(defun is-var (expr)
  "is this a variable (i.e. starts with ?)"
  (and (symbolp expr) (eql (char (symbol-name expr) 0) #\?)))

(defun find-vars (expr)
  "returns a list of all the variables in expr"
  (if (consp expr) (append (find-vars (car expr)) (find-vars (cdr expr)))
    (if (is-var expr) (list expr) nil)))

(defun make-explain-func (explain-stuff)
  "generates a function to generate explanation text at runtime"
  (let* ((explain-string (car explain-stuff))
         (explain-args (cdr explain-stuff))
         (vars (remove-duplicates (find-vars explain-args))))
    `#'(lambda (bindings)
         (declare (ignorable bindings))
         (let ,(mapcar #'(lambda (v) `(,v (cdr (assoc ',v bindings)))) vars)
           (format nil ,explain-string ,@explain-args)))))

(defmacro defexplain (rulename &key why what because)
  "puts an entry for the rule in the explanation table"
  `(setf (gethash ',rulename *explanation-table*)
         (list ,(make-explain-func why)
               ,(make-explain-func what)
	       ,(make-explain-func because))))

;;; next two definitions generate an explanation for each instantiation
;;; that fires and stores it away in *explanation*

(defun add-explanation (inst)
  "generate an explanation for firing this instantiation"
  (let ((explain-info (gethash (inst-rulename inst) *explanation-table*)))
    (when explain-info
	  (do-the-rest explain-info (inst-bindings inst) (inst-rulename inst)))))

(defun do-the-rest (explain-info bindings name)
  "creates explanation text derived from explain functions and bindings"
  (let ((why-func (first explain-info))
	(what-func (second explain-info))
	(because-func (third explain-info)))
    (push `(,*cycle* ,name
		     ,(funcall why-func bindings)
		     ,(funcall what-func bindings)
		     ,(funcall because-func bindings)) *explanation*)))

;;; meta-interpreter for explanation contexts
;;; before firing the rule generate explanation for this cycle
(defrule explain-context :backward
  ((explain-context)
   <--
   (start-cycle)
   (instantiation ?inst)
   ((add-explanation ?inst))
   (fire-rule ?inst)
   (cut)
   (explain-context)))

;;; simple text output of the explanation

(defun explain (&optional cycle)
  "print out either the whole explanation or just for one cycle"
  (if cycle (explain-cycle (assoc cycle *explanation*))
    (dolist (cycle-entry (reverse *explanation*))
      (explain-cycle cycle-entry))))

(defun explain-cycle (entry)
  "print this explanation entry"
  (if entry
      (let ((cycle (first entry))
            (rulename (second entry))
            (why (third entry))
            (what (fourth entry))
            (because (fifth entry)))
        (format t "~2%~a: ~a~%~a~%~a~%~a"
                cycle rulename why what because))
    (format t "~2%No explanation for this cycle")))

;;; we could make a really smart tool here, but to give the general idea...

(defun explain-an-action ()
  (let ((item (capi:prompt-with-list (reverse *explanation*)
                                     "Which action do you want explained?"
                                     :print-function 'fourth)))
    (if item (capi:display-message "~A" (fifth item)))))

;;; starting the rule interpreter should clear any old explanation

(defadvice (infer rest-explanation :before)
    (&rest args)
  (unless *in-interpreter* (setq *explanation* nil)))
