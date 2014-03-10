;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/elements:slider-print-function.lisp,v 1.1.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/elements/slider-print-function.lisp
;;
;; Does not work on Cocoa. 
;;
;; This example demonstrates using :PRINT-FUNCTION in a CAPI:SLIDER
;; to display values that are not linear integers.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-SLIDER-PRINT-FUNCTION)
;;
;; Then move the sliders. 


;; The top slider displays values from 0.010 to 0.650.
;; The bottom slider goes logarithmic from 1 to 10000.

;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")

(defun my-logarithmic-print-function (pane value)
  (declare (ignore pane))
  (let ((val (expt 10 (/ value 300))))
    (cond ((> 10 val)
           (format nil "~4,2f" val))
          ((> 100 val)
           (format nil "~4,1f" val))
          (t  (format nil "~d"   (round val))))))

(defun test-slider-print-function()
  (let ((fractional-slider 
         (make-instance 'capi:slider
                        :title "Fractional slider"
                        :start 10 :end 650
                        :print-function #'(lambda (pane value)
                                            (declare (ignore pane))
                                            (format nil "~5,3f"(/ value 1000)))))
        (logarithmic-slider 
         (make-instance 'capi:slider
                        :title "Logarithmic slider" 
                        :start 0 :end 1200
                        :print-function 'my-logarithmic-print-function)))
 
    (capi:contain 
     (make-instance 
      'capi:column-layout
      :description (list fractional-slider logarithmic-slider)))))
