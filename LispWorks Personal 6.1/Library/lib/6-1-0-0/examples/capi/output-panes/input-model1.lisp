;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/output-panes:input-model1.lisp,v 1.2.12.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/output-panes/input-model1.lisp
;;
;; This example demonstrates the inout model of an output pane
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-INPUT-MODEL1)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")

(defun TEST-INPUT-MODEL1 ()
  (capi:contain
                (make-instance
                 'capi:output-pane
                 :title "Press keys in the pane..."
                 :input-model '((:character draw-character)))
                :best-width 200 :best-height 200))

(defun draw-character (self x y character)
                 (gp:draw-character self character x y))
