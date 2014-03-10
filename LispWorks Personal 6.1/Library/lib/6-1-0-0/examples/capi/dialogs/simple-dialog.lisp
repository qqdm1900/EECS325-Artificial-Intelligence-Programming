;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/dialogs:simple-dialog.lisp,v 1.1.16.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/dialogs/simple-dialog.lisp
;;
;; This example demonstrates the use of dialogs in the CAPI.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-DIALOG)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")

(capi:define-interface dialog-test ()
  ()
  (:panes
   (input-pane
    capi:text-input-pane
    :accessor dialog-test-input-pane
    :callback-type :interface
    :callback 'exit-dialog-test)
   (buttons
    capi:push-button-panel
    :items '("OK" "Cancel")
    :layout-args '(:x-uniform-size-p t)
    :callback-type :interface
    :callbacks '(exit-dialog-test capi:abort-dialog)))
  (:layouts
   (default
    capi:column-layout
    '(input-pane buttons)
    :x-adjust :centre))
  (:default-initargs
   :title "Enter some text:"))

(defun exit-dialog-test (interface)
  (capi:exit-dialog
   (capi:text-input-pane-text
    (dialog-test-input-pane interface))))

(defun test-dialog ()
  (capi:display-dialog
   (make-instance 'dialog-test)))
