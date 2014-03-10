;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/choice:expanding-list.lisp,v 1.1.11.1 2011/08/24 13:26:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;----------------------------------------------------------------------------
;;
;; examples/capi/choice/expanding-list.lisp
;;
;; This example demonstrates the use of (setf capi:collection-items) and the
;; print-function in a list-panel.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-EXPANDING-LIST)
;;
;; Try the action gesture (double-click or press the Return key) on the items of the list-panel.


(in-package "CL-USER")

(capi:define-interface expanding-list ()
  ((expanded :initform nil :accessor expanding-list-expanded))
  (:panes
   (list
    capi:list-panel
    :items '(10 20 30 40 50 60 70 80)
    :visible-min-height :text-height
    :visible-min-width '(:character 8)
    :keep-selection-p t
    :action-callback 'expand-or-retract
    :print-function #'(lambda (x) 
                        (format nil "~v,' D" 
                                (if (zerop (rem x 10)) 3 5) x)))))

(defun generate-items (self)
  (let ((expanded (expanding-list-expanded self)))
    (loop for i from 10 to 80 by 10
          collect i
          when (member i expanded)
          nconcing (loop for i2 from (1+ i) to (+ i 9)
                         collect i2))))

(defun expand-or-retract (integer self)
  (with-slots (list) self
    (cond
     ((find integer (expanding-list-expanded self))
      (setf (expanding-list-expanded self)
            (remove integer (expanding-list-expanded self))))
     (t
      (push integer (expanding-list-expanded self))))
    (setf (capi:collection-items list)
          (generate-items self))))

(defun test-expanding-list () 
  (capi:contain (make-instance 'expanding-list)))
