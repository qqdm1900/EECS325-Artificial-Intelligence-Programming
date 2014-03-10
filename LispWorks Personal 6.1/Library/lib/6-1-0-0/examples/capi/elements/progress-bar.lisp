;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/elements:progress-bar.lisp,v 1.1.11.1 2011/08/24 13:26:21 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/elements/progress-bar.lisp
;;
;; This example demonstrates the uses of progress bars in the CAPI.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-PROGRESS-BARS)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")

(capi:define-interface progress-bar-test ()
  ()
  (:panes
   (progress-bar
    capi:progress-bar
    :start 50
    :end 100
    :title "Progress bar from 50 to 100"
    :title-position :frame
    )
   (controls
    capi:push-button-panel
    :items '(50 55 75 100)
    :print-function #'(lambda (item)
                        (format nil "Set to ~D" item))
    :selection-callback #'(lambda (data)
                            (setf (capi:range-slug-start progress-bar)
                                  data))
    :callback-type :data))
  (:layouts
   (main-layout
    capi:column-layout
    '(progress-bar controls)))
  (:default-initargs
   :title "Progress Bar Test"
   :best-width 300))


;;----------------------------------------------------------------------------
;; test-progress-bars
;;----------------------------------------------------------------------------

(defun test-progress-bars ()
  (capi:display (make-instance 'progress-bar-test)))

