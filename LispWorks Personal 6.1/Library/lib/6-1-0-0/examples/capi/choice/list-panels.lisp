;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/choice:list-panels.lisp,v 1.2.12.1 2011/08/24 13:26:22 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/choice/list-panels.lisp
;;
;; This example demonstrates the uses of list-panels in the CAPI.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-LIST-PANELS)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


;;----------------------------------------------------------------------------
;; Define the interface list-panel-test
;;----------------------------------------------------------------------------

(capi:define-interface list-panel-test ()
  ()
  (:panes
   (single-lp
    capi:list-panel
    :title "Single Selection:"
    :interaction :single-selection
    :items '("s1" "s2" "s3" "s4")
    :callback-type :data-interface
    :selection-callback 'list-panel-selection-callback
    :action-callback    'list-panel-action-callback
    :extend-callback    'list-panel-extend-callback
    :retract-callback   'list-panel-retract-callback)
   (multiple-lp
    capi:list-panel
    :title "Multiple Selection:"
    :interaction :multiple-selection
    :items '(m1 m2 m3 m4)
    :print-function 'string-downcase
    :callback-type :data-interface
    :selection-callback 'list-panel-selection-callback
    :action-callback    'list-panel-action-callback
    :extend-callback    'list-panel-extend-callback
    :retract-callback   'list-panel-retract-callback)
   (extend-lp 
    capi:list-panel
    :title "Extended Selection:"
    :interaction :extended-selection
    :items '(("e1" . 1) ("e2" . 2) ("e3" . 3) ("e4" . 4))
    :print-function 'first
    :data-function 'rest
    :callback-type :data-interface
    :selection-callback 'list-panel-selection-callback
    :action-callback    'list-panel-action-callback
    :extend-callback    'list-panel-extend-callback
    :retract-callback   'list-panel-retract-callback))
  (:layouts
   (main-layout
    capi:row-layout
    '(single-lp multiple-lp extend-lp)))
  (:default-initargs
   :title "List Panel Test"
   :best-width 500
   :best-height 300))


;;----------------------------------------------------------------------------
;; A generic callback
;;----------------------------------------------------------------------------

(defun list-panel-callback (type data interface)
  (capi:display-message
   "~S: ~A ~S"
   (capi:interface-title interface) type data))

(defun list-panel-selection-callback (&rest args)
  (apply 'list-panel-callback "selected" args))

(defun list-panel-action-callback (&rest args)
  (apply 'list-panel-callback "action on" args))

(defun list-panel-extend-callback (&rest args)
  (apply 'list-panel-callback "extended" args))

(defun list-panel-retract-callback (&rest args)
  (apply 'list-panel-callback "retract" args))


;;----------------------------------------------------------------------------
;; A simple test function
;;----------------------------------------------------------------------------

(defun test-list-panels ()
  (capi:display (make-instance 'list-panel-test)))
