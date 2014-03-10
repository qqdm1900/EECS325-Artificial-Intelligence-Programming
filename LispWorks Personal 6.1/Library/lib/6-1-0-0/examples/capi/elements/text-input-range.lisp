;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/elements:text-input-range.lisp,v 1.1.12.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/elements/text-input-range.lisp
;;
;; This example demonstrates the uses of text-input-range in the CAPI.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-TEXT-INPUT-RANGE)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


(capi:define-interface text-input-range-test ()
  ((last-min-max-value :initform 0)
   (last-value :initform 0)
   (setp :initform nil))
  (:panes
   (text-input-range
    capi:text-input-range
    :accessor text-input-range
    :start 0
    :value 0
    :end 100
    :callback 'text-input-range-test-callback
    :callback-type :interface-data)
   (history-pane
    capi:display-pane
    :accessor history-pane
    :visible-min-height '(character 2)
    :visible-max-width nil
    :text '("Initial value 0")))
  (:layouts
   (default-layout
    capi:grid-layout
    '("Adjust value:" text-input-range
      "History:" history-pane)
    :y-adjust :center))
  (:default-initargs
   :title "Text Input Range Test"
   :best-width 300))


;;----------------------------------------------------------------------------
;; Callbacks
;;----------------------------------------------------------------------------

(defun text-input-range-test-callback (interface value)
  (with-slots (setp last-value last-min-max-value) interface
    (let* ((history-pane (history-pane interface))
           (new-text-p (shiftf setp nil))
           (text
            (cond ((= value (1+ last-value))
                   (when (< last-value last-min-max-value)
                     (setf last-min-max-value last-value
                           new-text-p t))
                   (format nil "Increasing from ~D to ~D"
                           last-min-max-value value))
                  ((= value (1- last-value))
                   (when (> last-value last-min-max-value)
                     (setf last-min-max-value last-value
                           new-text-p t))
                   (format nil "Decreasing from ~D to ~D"
                           last-min-max-value value))
                  (t
                   (setq new-text-p (/= last-min-max-value last-value))
                   (setf last-min-max-value value
                         setp t)
                   (format nil "Set to ~D"
                           value)))))
      (setf (capi:display-pane-text history-pane)
            (if new-text-p
                (list (car (last (capi:display-pane-text history-pane)))
                      text)
              (list (first (capi:display-pane-text history-pane))
                    text)))
      (setf last-value value))))


;;----------------------------------------------------------------------------
;; test-text-input-range
;;----------------------------------------------------------------------------

(defun test-text-input-range ()
  (capi:display (make-instance 'text-input-range-test)))
