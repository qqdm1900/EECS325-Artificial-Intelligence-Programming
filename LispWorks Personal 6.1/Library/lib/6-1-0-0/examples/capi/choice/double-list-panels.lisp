;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/choice:double-list-panels.lisp,v 1.4.2.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/choice/double-list-panels.lisp
;;
;; This example demonstrates the uses of capi:DOUBLE-LIST-PANEL in the CAPI.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-DOUBLE-LIST-PANELS)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")

;;; Define some random image locators to demostrate the use
;;; of :IMAGE-FUNCTION in double-list-panel. 
(defvar *double-list-panel-image-set* (capi:make-general-image-set
                                       :id #.(gp:read-external-image
                                              (current-pathname"tree.bmp"))
                                       :image-count 4
                                       :width 16
                                       :height 16))

(defvar *double-list-panel-image-locators*
  (apply 'vector 
         (loop for  x below 4 collect 
               (capi:make-image-locator 
                :image-set *double-list-panel-image-set*
                :index x))))

;;----------------------------------------------------------------------------
;; Define the interface double-list-panel-test
;;----------------------------------------------------------------------------

(capi:define-interface double-list-panel-test ()
  ()
  (:panes
   (single-lp
    capi:double-list-panel
    :title "Single Selection:"
    :interaction :single-selection
    :items '("s1" "s2" "s3" "s4")
    :callback-type :data-interface
    :selection-callback 'double-list-panel-selection-callback
    :action-callback    'double-list-panel-action-callback
    :extend-callback    'double-list-panel-extend-callback
    :retract-callback   'double-list-panel-retract-callback)
   (multiple-lp
    capi:double-list-panel
    :title "Multiple Selection:"
    :interaction :multiple-selection
    :items '(m1 m2 m3 m4)
    :print-function 'string-downcase
    :callback-type :data-interface
    :selection-callback 'double-list-panel-selection-callback
    :action-callback    'double-list-panel-action-callback
    :extend-callback    'double-list-panel-extend-callback
    :retract-callback   'double-list-panel-retract-callback)
   (extend-lp 
    capi:double-list-panel
    :title "Extended Selection:"
    :interaction :extended-selection
    :items '(("e1" . 1) ("e2" . 2) ("e3" . 3) ("e4" . 4))
    :print-function 'first
    :data-function 'cdr
    :callback-type :data-interface
    :image-function #'(lambda (pair) 
                        (svref *double-list-panel-image-locators* (1- (cdr pair))))
    :selection-callback 'double-list-panel-selection-callback
    :action-callback    'double-list-panel-action-callback
    :extend-callback    'double-list-panel-extend-callback
    :retract-callback   'double-list-panel-retract-callback)
  (output
   capi:display-pane
   :visible-max-width nil))
  (:layouts
   (main-layout
    capi:column-layout
    '(single-lp multiple-lp extend-lp output)))
  (:default-initargs
   :title "Double-List Panel Test"
   :best-width 500
   :best-height 300))


;;----------------------------------------------------------------------------
;; A generic callback
;;----------------------------------------------------------------------------

(defun double-list-panel-callback (type data double-list-panel)
  ;;; capi:DOUBLE-LIST-PANEL inherits the TITLE from capi:INTERFACE,
  ;;; which is why capi:interface-title is used to get the title. 
  (let ((title (capi:interface-title double-list-panel)))
    (with-slots (output) (capi:top-level-interface double-list-panel)
      (setf (capi:display-pane-text output)
            (format nil "~a ~A ~S" title type data)))))

(defun double-list-panel-selection-callback (&rest args)
  (apply 'double-list-panel-callback "selected" args))

(defun double-list-panel-action-callback (&rest args)
  (apply 'double-list-panel-callback "action on" args))

(defun double-list-panel-extend-callback (&rest args)
  (apply 'double-list-panel-callback "extended" args))

(defun double-list-panel-retract-callback (&rest args)
  (apply 'double-list-panel-callback "retract" args))


;;----------------------------------------------------------------------------
;; A simple test function
;;----------------------------------------------------------------------------

(defun test-double-list-panels ()
  (capi:display (make-instance 'double-list-panel-test)))
