;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/elements:cursor.lisp,v 1.2.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;; Copyright (c) 1987--2009 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

;;; ----------------------------------------------------------------------
;;;
;;; Example use of mouse cursors.
;;;
;;; To try it, compile and load this file and then execute:
;;;
;;;      (CL-USER::TEST-MOUSE-CURSOR)
;;;
;;; Click in the green area to change the cursor.
;;; See documentation for capi:simple-pane-cursor.
;;; 
;;; ----------------------------------------------------------------------

(capi:define-interface mouse-cursor-example ()
  ()
  (:panes
   (cursor-pane
    capi:output-pane
    :input-model '(((:button-1 :press) pane-change-mouse-cursor))
    :background (color:make-rgb 0.0 0.5 0.0)
    :visible-min-width 300
    :visible-min-height 300)
   (cursor-name-pane
    capi:display-pane
    :visible-max-width nil)
   (tp
    capi:title-pane 
    :visible-max-width nil
    :text "Click in the green area to change the cursor")
  ))

;;; All the cursor names that are available on all platforms. 
(defvar *curesor-names*
  #(nil :i-beam :top-left-arrow :h-double-arrow
    :v-double-arrow :left-side :right-side 
    :top-side :bottom-side 
    :crosshair :hand :fleur :move))

(defvar *index* 0)

(defun pane-change-mouse-cursor (pane x y)
  (declare (ignore x y))
  (change-mouse-cursor (capi:top-level-interface pane)))

(defun change-mouse-cursor (interface)
  (let ((index (incf *index*))
        (vec *curesor-names*))
    (when (>= index (length vec))
      (setq index (setq *index* 0)))
    (let ((cursor-name (svref vec index)))
      (with-slots (cursor-pane cursor-name-pane) interface
        (setf (capi:simple-pane-cursor cursor-pane)
              cursor-name)
        (setf (capi:display-pane-text cursor-name-pane)
              (format nil "Cursor-name : ~s"  cursor-name)
              )))))

(defun test-mouse-cursor ()
  (capi:display (make-instance 'mouse-cursor-example)))

