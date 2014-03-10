;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/elements:convert-relative-position.lisp,v 1.2.9.1 2011/08/24 13:26:21 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/elements/convert-relative-position.lisp
;;
;; This example demonstrates the use CAPI:CONVERT-RELATIVE-POSITION.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-CONVERT-RELATIVE-POSITION-DEMO)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")

(defvar *convert-relative-position-demo-input-model*
  '(((:button-1 :press) convert-relative-position-demo-click)))

(capi:define-interface convert-relative-position-demo ()
  ((last-pane :initform nil)
   (last-x :initform nil)
   (last-y :initform nil))
  (:panes
   (title capi:title-pane
          :text "Click on the panes and see the coordinates in the bottom.")
   (click1 capi:output-pane
           :input-model *convert-relative-position-demo-input-model*)
   (click2 capi:output-pane
           :input-model *convert-relative-position-demo-input-model*)
   (click3 capi:output-pane
           :input-model *convert-relative-position-demo-input-model*
           :vertical-scroll t
           :horizontal-scroll t
           :scroll-width 500
           :scroll-height 500)
   (click4 capi:output-pane
           :input-model *convert-relative-position-demo-input-model*)
   (relative-to capi:radio-button-panel
                :items '(:pane :layout :window :screen)
                :print-function 'string-capitalize
                :title "Show relative to:"
                :title-position :left
                :selection-callback 'convert-relative-position-demo-relative-to
                :callback-type :interface)
   (message capi:display-pane :visible-max-width nil))
  (:layouts
   (main-layout capi:column-layout
                '(title click-layout relative-to message)
                :default t)
   (click-layout capi:grid-layout
                 '(click1 click2 click3 click4)
                 :columns 2
                 :visible-border t))
  (:default-initargs
   :best-width 400
   :best-height 400
   :title "Convert Relative Position Demo"))


;;----------------------------------------------------------------------------
;; Callbacks
;;----------------------------------------------------------------------------

(defun convert-relative-position-demo-click (pane x y)
  (with-slots (message relative-to last-pane last-x last-y)
      (capi:element-interface pane)
    (let ((relative-to-choice (capi:choice-selected-item relative-to)))
      (multiple-value-bind (rel-x rel-y)
          (capi:convert-relative-position
           pane
           (case relative-to-choice
             (:pane pane)
             (:layout (capi:element-parent pane))
             (:window (capi:top-level-interface pane))
             (:screen (capi:element-screen pane)))
           x
           y)
        (setf (capi:display-pane-text message)
              (format nil "Clicked in ~A at ~D,~D giving ~D,~D from ~A."
                      (capi:capi-object-name pane)
                      x y
                      rel-x rel-y
                      (string-downcase relative-to-choice)))
        (setf last-pane pane last-x x last-y y)))))

(defun convert-relative-position-demo-relative-to (interface)
  (with-slots (last-pane last-x last-y) interface
    (when last-pane
      (convert-relative-position-demo-click last-pane last-x last-y))))

;;----------------------------------------------------------------------------
;; Main function
;;----------------------------------------------------------------------------

(defun test-convert-relative-position-demo ()
  (capi:display (make-instance 'convert-relative-position-demo)))
  
