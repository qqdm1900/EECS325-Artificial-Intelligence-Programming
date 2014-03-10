;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/editor:editor-pane.lisp,v 1.10.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;----------------------------------------------------------------------------
;;
;; examples/capi/editor/editor-pane.lisp
;;
;; This example demonstrates the uses of editor-panes in the CAPI.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-EDITOR-PANE)
;;
;; The example also demonstrates how to create an editor-pane
;; that cannot be edited directly. To do this, execute:
;;
;;      (CL-USER::TEST-EDITOR-PANE t) 
;;
;; The result is an editor-pane that you can navigate around, but not
;; edit directly. You can still edit it using the buttons. 
;;----------------------------------------------------------------------------

(in-package "CL-USER")


(defvar *editor-text*
    ";;----------------------------------------------------------------------------
;;
;; examples/capi/editor/editor-pane.lisp
;;
;; This example demonstrates the uses of editor-panes in the CAPI.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-EDITOR-PANE)
;;
;;----------------------------------------------------------------------------
")


(capi:define-interface editor-pane-test ()
  ()
  (:panes
   (editor-pane
    capi:editor-pane
    :text *editor-text*
    :echo-area-pane echo-area
    :visible-min-width '(character 80)
    :visible-min-height '(character 15))
   (buttons
    capi:push-button-panel
    :items '("Beginning Of Buffer" "End Of Buffer" "Kill Line" "Undo")
    :callback-type :data
    :selection-callback #'(lambda (command)
                            (capi:call-editor editor-pane command)))
   (echo-area capi:echo-area-pane :max-height t))
  (:default-initargs
   :title "Editor Pane Test"))

(defun test-editor-pane (&optional read-only)
  (let ((ept (make-instance 'editor-pane-test)))
    (when read-only
      (with-slots (editor-pane) ept
          (setf (capi:simple-pane-enabled editor-pane) :read-only)))
  (capi:display ept)))
