;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/choice:option-pane.lisp,v 1.7.1.1 2011/08/24 13:26:21 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;----------------------------------------------------------------------------
;;
;; examples/capi/choice/option-pane.lisp
;;
;; This example demonstrates simple OPTION-PANE.
;; To try it, compile and load this file and then execute:
;;

;;      (CL-USER::TEST-OPTION-PANE)

;; On GTK it requires 2.6 or later to work properly. In earlier versions
;; the separator is not really implemented, and disabled items are
;; properly disabled.

;; When you select a color, the callback (option-pane-set-color) sets
;; the color of lower rectangle.

;; The radio button demostrate how to use capi:option-pane-enabled-positions
;; to enable only some of the options. 
;;----------------------------------------------------------------------------

(in-package "CL-USER")

(defvar *color-show-options* 
  '("Allow all colors" "Allow primary only"))

;;----------------------------------------------------------------------------
;; Define the interface option-pane-test
;;----------------------------------------------------------------------------

(capi:define-interface option-pane-test ()
  () 
  (:panes
   (color-chooser
    capi:option-pane
    :accessor color-chooser
    :title "Select Color:"
    :separator-item :separator
    :items '(:red :green :blue 
             :separator   ; recognized because of :separator-item above
             :white :black :pink :gold :cyan)
    :print-function 'string-capitalize
    :selection-callback 'option-pane-set-color)
   (color-kind
    capi:radio-button-panel
    :layout-class 'capi:column-layout  ;; make it vertical
    :accessor color-kind
    :items *color-show-options*
    :selection-callback 'option-pane-set-color-kind)
   (color-square
    capi:output-pane
    :accessor color-square))
  (:layouts
   (default-layout
    capi:column-layout
    '(color-chooser color-kind color-square)))
  (:default-initargs
   :title "Option Pane Test"
   :best-height 200))

(defmethod initialize-instance :after ((self option-pane-test) &key color)
  (option-pane-set-color (or color
                             (capi:choice-selected-item (color-chooser self)))
                         self))


;;----------------------------------------------------------------------------
;; A generic callback
;;----------------------------------------------------------------------------

(defun option-pane-set-color (color interface)
  (setf (capi:simple-pane-background (color-square interface)) color))

;;; The callback of the radio panel.

(defun option-pane-set-color-kind (kind interface)
  (setf (capi:option-pane-enabled-positions (color-chooser interface))
        (if (eq kind (car *color-show-options*))
            :all
          '(0 1 2))))


;;----------------------------------------------------------------------------
;; A simple test function
;;----------------------------------------------------------------------------

(defun test-option-pane ()
  (capi:display (make-instance 'option-pane-test)))
